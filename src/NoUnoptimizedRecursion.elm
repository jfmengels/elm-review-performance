module NoUnoptimizedRecursion exposing
    ( rule
    , Configuration, optInWithComment, optOutWithComment
    )

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports recursive functions that are not tail-call optimized.

TODO Give good resource for TCO

    -- Reports recursive functions by default, opt out with a comment containing "IGNORE TCO"
    config =
        [ NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
        ]

    -- Reports only the functions with a comment containing "CHECK TCO"
    config =
        [ NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optInWithComment "CHECK TCO")
        ]

You can use comments to say that a function should or should not be checked, depending on the configuration method you chose.
This comment has to appear on the line after the `=` that follows the declaration of your function, and will not report the function.
The same will apply for functions defined in a let expression.

    fun n =
        -- elm-review: IGNORE TCO
        if condition n then
            fun n * n

        else
            n

I recommend toggling between the two configuration options while you're fixing/ignoring the existing issues, and to use the
opt-out configuration afterwards.

I recommend to **not** default to ignoring a reported issue, and to discuss with your colleagues how to best solve the
problem when you encounter the issue or when you see them ignore an error.

TODO Indicate in the error that you can opt out using the comment


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful for both application maintainers and package authors to detect locations where
performance could be improved and where stack overflows can happen.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-performance/example --rules NoUnoptimizedRecursion
```

The rule uses `optOutWithComment "IGNORE TCO"` as its configuration.

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


type Configuration
    = OptOut String
    | OptIn String


optOutWithComment : String -> Configuration
optOutWithComment comment =
    OptOut comment


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
