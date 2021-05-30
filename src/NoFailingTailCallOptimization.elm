module NoFailingTailCallOptimization exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ NoFailingTailCallOptimization.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-performance/example --rules NoFailingTailCallOptimization
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoFailingTailCallOptimization" initialContext
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { currentFunctionName : String
    , tcoLocations : List Range
    , newScopes : List ( Range, String )
    , parentScopes : List ( Range, Scope )
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
    , newScopes = []
    , parentScopes = []
    }


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            ( []
            , { currentFunctionName =
                    function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
              , tcoLocations =
                    [ function.declaration
                        |> Node.value
                        |> .expression
                        |> Node.range
                    ]
              , newScopes = []
              , parentScopes = []
              }
            )

        _ ->
            ( [], context )


expressionEnterVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionEnterVisitor node context =
    let
        newContext : Context
        newContext =
            case context.newScopes of
                [] ->
                    context

                ( range, name ) :: rest ->
                    if range == Node.range node then
                        { currentFunctionName = name
                        , tcoLocations = [ range ]
                        , newScopes = rest
                        , parentScopes =
                            ( range
                            , { currentFunctionName = context.currentFunctionName, tcoLocations = context.tcoLocations, newScopes = rest }
                            )
                                :: context.parentScopes
                        }

                    else
                        context
    in
    if isInTcoLocation context (Node.range node) then
        ( [], addAllowedLocation node newContext )

    else
        ( reportRecursiveCallInNonAllowedLocation node newContext, newContext )


reportRecursiveCallInNonAllowedLocation : Node Expression -> Context -> List (Rule.Error {})
reportRecursiveCallInNonAllowedLocation node context =
    case Node.value node of
        Expression.Application ((Node funcRange (Expression.FunctionOrValue [] name)) :: _) ->
            if name == context.currentFunctionName then
                [ Rule.error
                    { message = "REPLACEME"
                    , details = [ "REPLACEME" ]
                    }
                    funcRange
                ]

            else
                []

        _ ->
            []


addAllowedLocation : Node Expression -> Context -> Context
addAllowedLocation node context =
    case Node.value node of
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
                                        , Node.value functionDeclaration.name
                                        )

                                Expression.LetDestructuring _ _ ->
                                    Nothing
                        )
                        declarations
            in
            { context
                | newScopes = newScopes

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
                  , newScopes = headScope.newScopes
                  , parentScopes = restOfParentScopes
                  }
                )

            else
                ( [], context )


isInTcoLocation : Context -> Range -> Bool
isInTcoLocation context range =
    List.member range context.tcoLocations
