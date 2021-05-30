module NoFailingTailCallOptimization exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
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
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { currentFunction : String
    }


initialContext : { currentFunction : String }
initialContext =
    { currentFunction = ""
    }


declarationVisitor : Node Declaration -> Context -> ( List nothing, Context )
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration function ->
            ( []
            , { currentFunction =
                    function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
              }
            )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            if name == context.currentFunction then
                ( [ Rule.error
                        { message = "REPLACEME"
                        , details = [ "REPLACEME" ]
                        }
                        (Node.range node)
                  ]
                , context
                )

            else
                ( [], context )

        _ ->
            ( [], context )
