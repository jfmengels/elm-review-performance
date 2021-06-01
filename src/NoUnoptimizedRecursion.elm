module NoUnoptimizedRecursion exposing
    ( rule
    , Configuration, optOutWithComment, optInWithComment
    )

{-|

@docs rule

Tail-call optimization makes Elm code more performant and helps prevent stack overflows.

Since this optimization is done silently and under specific circumstances, it is unfortunately relatively easy
to not notice when the optimization is not being applied. You can find the [reasons why a function would not be optimized below](#fail).


## Configuration

@docs Configuration, optOutWithComment, optInWithComment


## When (not) to enable this rule

This rule is useful for both application maintainers and package authors to detect locations where
performance could be improved and where stack overflows can happen.

You should not enable this rule if you currently do not want to invest your time into thinking about performance.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-performance/example --rules NoUnoptimizedRecursion
```

The rule uses `optOutWithComment "IGNORE TCO"` as its configuration.


## Success

Not reported because it is tail-call optimized.

    fun n =
        if condition n then
            fun (n - 1)

        else
            n

Not reported because the function has been tagged as ignored.

    -- With opt-out configuration
    config =
        [ NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
        ]

    fun n =
        -- elm-review: IGNORE TCO
        fun n * n

Not reported because the function has not been tagged.

    -- With opt-in configuration
    config =
        [ NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optInWithComment "CHECK TCO")
        ]

    fun n =
        fun n * n


## Fail

To understand when a function would not get tail-call optimized, it is important to understand when it would be optimized.

The Elm compiler is able to do tail-call elimination **only** when all the recursive calls are the last operation that the function would do in that branch. Any recursive calls not happening in those locations de-optimizes the function.

Here are the locations when a recursive call may happen in:

  - branches of an if expression
  - branches of a case expression
  - in the body of a let expression (but not the functions)

only if each of the above appeared at the root of the function or in one of the above locations themselves.

Following is a list of likely situations that will be reported.


### An operation is applied on the result of a function call

The result of this function gets multiplied by `n`, making the recursive call not the last thing to happen in this branch.

    factorial : Int -> Int
    factorial n =
        if n <= 1 then
            1

        else
            factorial (n - 1) * n

Hint: When you need to apply a function on the result of a recursive call, what you can often do is to add an argument holding the result value and apply the operations on it instead.

    factorialHelp : Int -> Int -> Int
    factorialHelp n result =
        if n <= 1 then
            result

        else
            factorial (result * n) * n

And split the function into the one that will do recursive calls (above) and a "public-facing" function which will set the initial result value (below).

    factorial : Int -> Int
    factorial n =
        factorialHelp n 1


### Calls using the |> or <| operators

The following won't be optimized. The fix here is very easy, as you only need to remove the operator and use parentheses instead.

    fun n =
        if condition n then
            fun <| n - 1

        else
            n
    fun n =
        if condition n then
            (n - 1)
                |> fun

        else
            n


### Calls appearing in || or && conditions

The following won't be optimized.

    isPrefixOf : List a -> List a -> Bool
    isPrefixOf prefix list =
        case ( prefix, list ) of
            ( [], _ ) ->
                True

            ( _ :: _, [] ) ->
                False

            ( p :: ps, x :: xs ) ->
                p == x && isPrefixOf ps xs

The fix here is very easy, as you only need to use if expressions instead.

    isPrefixOf : List a -> List a -> Bool
    isPrefixOf prefix list =
        case ( prefix, list ) of
            ( [], _ ) ->
                True

            ( _ :: _, [] ) ->
                False

            ( p :: ps, x :: xs ) ->
                if p == x then
                    isPrefixOf ps xs

                else
                    False


### Calls from let declarations

    fun n =
        let
            funHelp y =
                fun (y - 1)
        in
        funHelp n

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports recursive functions that are not [tail-call optimized](https://functional-programming-in-elm.netlify.app/recursion/tail-call-elimination.html).
-}
rule : Configuration -> Rule
rule configuration =
    Rule.newModuleRuleSchema "NoUnoptimizedRecursion" initialContext
        |> Rule.withCommentsVisitor (commentsVisitor configuration)
        |> Rule.withDeclarationEnterVisitor (declarationVisitor configuration)
        |> Rule.withExpressionEnterVisitor (expressionEnterVisitor configuration)
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.fromModuleRuleSchema



-- CONFIGURATION


{-| Configuration for `NoUnoptimizedRecursion`.

Use [`optOutWithComment`](#optOutWithComment) or [`optInWithComment`](#optInWithComment) to configure this rule.

You can use comments to tag functions as to be checked or ignored, depending on the configuration option you chose.
This comment has to appear on the line after the `=` that follows the declaration of your function. Note that this
comment only needs to contain the tag that you're choosing and that it is case-sensitive.
The same will apply for functions defined in a let expression, since they can be tail-call optimized as well.

I recommend toggling between the two configuration options while you're fixing/ignoring the existing issues, and to use the
opt-out configuration afterwards.

-}
type Configuration
    = OptOut String
    | OptIn String


{-| Reports recursive functions by default, opt out functions tagged with a comment.

    config =
        [ NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
        ]

With the configuration above, the following function would not be reported.

    fun n =
        -- elm-review: IGNORE TCO
        if condition n then
            fun n * n

        else
            n

I recommend to **not** default to ignoring a reported issue, and instead to discuss with your colleagues how to best
solve the error when you encounter it or when you see them ignore an error.

-}
optOutWithComment : String -> Configuration
optOutWithComment comment =
    OptOut comment


{-| Reports only the functions tagged with a comment.

    config =
        [ NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optInWithComment "CHECK TCO")
        ]

With the configuration above, the following function would be reported.

    fun n =
        -- CHECK TCO
        if condition n then
            fun n * n

        else
            n

-}
optInWithComment : String -> Configuration
optInWithComment comment =
    OptIn comment


shouldReportFunction : Configuration -> Context -> Range -> Bool
shouldReportFunction configuration context range =
    let
        foundComment : Bool
        foundComment =
            Set.member (range.start.row + 1) context.comments
    in
    case configuration of
        OptOut _ ->
            not foundComment

        OptIn _ ->
            foundComment



-- CONTEXT


type alias Context =
    { currentFunctionName : String
    , tcoLocations : List Range
    , newScopesForLet : List ( Range, String )
    , parentScopes : List ( Range, Scope )
    , parentNames : Set String
    , comments : Set Int
    }


type alias Scope =
    { currentFunctionName : String
    , tcoLocations : List Range
    , newScopes : List ( Range, String )
    }


initialContext : Context
initialContext =
    { currentFunctionName = ""
    , tcoLocations = []
    , newScopesForLet = []
    , parentScopes = []
    , parentNames = Set.empty
    , comments = Set.empty
    }


commentsVisitor : Configuration -> List (Node String) -> Context -> ( List nothing, Context )
commentsVisitor configuration comments context =
    let
        commentTag : String
        commentTag =
            case configuration of
                OptOut commentTag_ ->
                    commentTag_

                OptIn commentTag_ ->
                    commentTag_
    in
    ( []
    , { context
        | comments =
            comments
                |> List.filter (Node.value >> String.contains commentTag)
                |> List.map (Node.range >> .start >> .row)
                |> Set.fromList
      }
    )


declarationVisitor : Configuration -> Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor configuration node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            ( []
            , { currentFunctionName =
                    if shouldReportFunction configuration context (Node.range function.declaration) then
                        function.declaration
                            |> Node.value
                            |> .name
                            |> Node.value

                    else
                        ""
              , tcoLocations =
                    [ function.declaration
                        |> Node.value
                        |> .expression
                        |> Node.range
                    ]
              , newScopesForLet = []
              , parentScopes = []
              , parentNames = Set.empty
              , comments = context.comments
              }
            )

        _ ->
            ( [], context )


expressionEnterVisitor : Configuration -> Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionEnterVisitor configuration node context =
    let
        newContext : Context
        newContext =
            case context.newScopesForLet of
                [] ->
                    context

                ( range, name ) :: restOfNewScopes ->
                    if range == Node.range node then
                        { currentFunctionName = name
                        , tcoLocations = [ range ]
                        , newScopesForLet = restOfNewScopes
                        , parentScopes =
                            ( range
                            , { currentFunctionName = context.currentFunctionName, tcoLocations = context.tcoLocations, newScopes = restOfNewScopes }
                            )
                                :: context.parentScopes
                        , parentNames = Set.insert context.currentFunctionName context.parentNames
                        , comments = context.comments
                        }

                    else
                        context
    in
    if isInTcoLocation newContext (Node.range node) then
        ( reportReferencesToParentFunctions node newContext, addAllowedLocation configuration node newContext )

    else
        ( reportRecursiveCallInNonAllowedLocation node newContext, newContext )


reportRecursiveCallInNonAllowedLocation : Node Expression -> Context -> List (Rule.Error {})
reportRecursiveCallInNonAllowedLocation node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            if name == context.currentFunctionName then
                [ Rule.error
                    { message = "Recursive function is not tail-call optimized"
                    , details = [ "The way this function is called recursively here prevents the function from being tail-call optimized." ]
                    }
                    (Node.range node)
                ]

            else
                []

        _ ->
            []


reportReferencesToParentFunctions : Node Expression -> Context -> List (Rule.Error {})
reportReferencesToParentFunctions node context =
    case Node.value node of
        Expression.Application ((Node funcRange (Expression.FunctionOrValue [] name)) :: _) ->
            if Set.member name context.parentNames then
                [ Rule.error
                    { message = "Recursive function is not tail-call optimized"
                    , details = [ "The way this function is called recursively here prevents the function from being tail-call optimized." ]
                    }
                    funcRange
                ]

            else
                []

        _ ->
            []


addAllowedLocation : Configuration -> Node Expression -> Context -> Context
addAllowedLocation configuration node context =
    case Node.value node of
        Expression.Application (function :: _) ->
            { context | tcoLocations = Node.range function :: context.tcoLocations }

        Expression.IfBlock _ thenBranch elseBranch ->
            { context | tcoLocations = Node.range thenBranch :: Node.range elseBranch :: context.tcoLocations }

        Expression.LetExpression { declarations, expression } ->
            let
                newScopes : List ( Range, String )
                newScopes =
                    List.filterMap
                        (\decl ->
                            case Node.value decl of
                                Expression.LetFunction function ->
                                    let
                                        functionDeclaration : Expression.FunctionImplementation
                                        functionDeclaration =
                                            Node.value function.declaration
                                    in
                                    Just
                                        ( Node.range functionDeclaration.expression
                                        , if shouldReportFunction configuration context (Node.range function.declaration) then
                                            Node.value functionDeclaration.name

                                          else
                                            ""
                                        )

                                Expression.LetDestructuring _ _ ->
                                    Nothing
                        )
                        declarations
            in
            { context
                | newScopesForLet = newScopes

                {- The following translates to TCO code

                   let
                       fun x =
                          fun x
                   in
                   fun 1
                -}
                , tcoLocations = Node.range expression :: context.tcoLocations
            }

        Expression.ParenthesizedExpression expr ->
            {- The following translates to TCO code

               fun x =
                 (fun x)
            -}
            { context | tcoLocations = Node.range expr :: context.tcoLocations }

        Expression.CaseExpression { cases } ->
            let
                caseBodies : List Range
                caseBodies =
                    List.map (Tuple.second >> Node.range) cases
            in
            { context | tcoLocations = caseBodies ++ context.tcoLocations }

        _ ->
            context


expressionExitVisitor : Node Expression -> Context -> ( List nothing, Context )
expressionExitVisitor node context =
    case context.parentScopes of
        [] ->
            ( [], context )

        ( headRange, headScope ) :: restOfParentScopes ->
            if headRange == Node.range node then
                ( []
                , { currentFunctionName = headScope.currentFunctionName
                  , tcoLocations = headScope.tcoLocations
                  , newScopesForLet = headScope.newScopes
                  , parentScopes = restOfParentScopes
                  , parentNames = Set.remove headScope.currentFunctionName context.parentNames
                  , comments = context.comments
                  }
                )

            else
                ( [], context )


isInTcoLocation : Context -> Range -> Bool
isInTcoLocation context range =
    List.member range context.tcoLocations
