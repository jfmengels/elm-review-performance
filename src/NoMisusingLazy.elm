module NoMisusingLazy exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)



-- TODO Report lazy being used on its own
-- TODO Handle calls through operators
-- TODO Support Element.lazy and other known lazy functions (https://klaftertief.github.io/elm-search/?q=lazy)


{-| Reports... REPLACEME

    config =
        [ NoMisusingLazy.rule
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
elm-review --template jfmengels/elm-review-performance/example --rules NoMisusingLazy
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoMisusingLazy" initialContext
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , functionHasArguments : Bool
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , functionHasArguments = False
            }
        )
        |> Rule.withModuleNameLookupTable


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            let
                hasArguments : Bool
                hasArguments =
                    function.declaration
                        |> Node.value
                        |> .arguments
                        |> List.isEmpty
                        |> not
            in
            ( []
            , { context | functionHasArguments = hasArguments }
            )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.Application ((Node functionRange (Expression.FunctionOrValue _ "lazy")) :: _) ->
            case ModuleNameLookupTable.moduleNameAt context.lookupTable functionRange of
                Just [ "Html", "Lazy" ] ->
                    if context.functionHasArguments then
                        ( [ Rule.error
                                { message = "Misuse of a lazy function"
                                , details = [ "REPLACEME" ]
                                }
                                functionRange
                          ]
                        , context
                        )

                    else
                        ( [], context )

                _ ->
                    ( [], context )

        _ ->
            ( [], context )
