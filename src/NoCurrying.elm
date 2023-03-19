module NoCurrying exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ NoCurrying.rule
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
elm-review --template jfmengels/elm-review-performance/example --rules NoCurrying
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoCurrying" initialContext
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias ModuleContext =
    { functionArity : FunctionArityDict
    }


type alias FunctionArityDict =
    Dict String Int


initialContext : ModuleContext
initialContext =
    { functionArity = Dict.fromList [ ( "function", 2 ) ]
    }


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationListVisitor declarations context =
    let
        functionArity : FunctionArityDict
        functionArity =
            List.foldl inferArityForDeclaration context.functionArity declarations
    in
    ( [], { context | functionArity = functionArity } )


inferArityForDeclaration : Node Declaration -> FunctionArityDict -> FunctionArityDict
inferArityForDeclaration node dict =
    case Node.value node of
        Declaration.FunctionDeclaration { declaration } ->
            inferArityForFunction (Node.value declaration) dict

        _ ->
            dict


inferArityForFunction : Expression.FunctionImplementation -> FunctionArityDict -> FunctionArityDict
inferArityForFunction function dict =
    if List.isEmpty function.arguments then
        dict

    else
        Dict.insert (Node.value function.name) (List.length function.arguments) dict


expressionVisitor : Node Expression -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Expression.Application ((Node functionRange (Expression.FunctionOrValue [] name)) :: arguments) ->
            case Dict.get name context.functionArity of
                Just expectedNbArguments ->
                    if List.length arguments < expectedNbArguments then
                        ( [ Rule.error
                                { message = "REPLACEME"
                                , details = [ "REPLACEME" ]
                                }
                                functionRange
                          ]
                        , context
                        )

                    else
                        ( [], context )

                Nothing ->
                    ( [], context )

        _ ->
            ( [], context )
