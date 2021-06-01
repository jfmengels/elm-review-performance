module NoMisusingLazy exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)



-- TODO Report lazy being used on its own
-- TODO Handle calls through operators
-- TODO SUpport Element.lazy, and other known lazy functions (https://klaftertief.github.io/elm-search/?q=lazy)


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
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            }
        )
        |> Rule.withModuleNameLookupTable


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.Application ((Node functionRange (Expression.FunctionOrValue _ "lazy")) :: _) ->
            case ModuleNameLookupTable.moduleNameAt context.lookupTable functionRange of
                Just [ "Html", "Lazy" ] ->
                    ( [ Rule.error
                            { message = "Misuse of a lazy function"
                            , details = [ "REPLACEME" ]
                            }
                            functionRange
                      ]
                    , context
                    )

                _ ->
                    ( [], context )

        _ ->
            ( [], context )
