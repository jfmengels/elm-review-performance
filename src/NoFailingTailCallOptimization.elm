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
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { currentFunctionName : String
    , tcoLocations : List Range
    }


initialContext : Context
initialContext =
    { currentFunctionName = ""
    , tcoLocations = []
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
              }
            )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.Application ((Node funcRange (Expression.FunctionOrValue [] name)) :: _) ->
            if name == context.currentFunctionName && not (isInTcoLocation context (Node.range node)) then
                ( [ Rule.error
                        { message = "REPLACEME"
                        , details = [ "REPLACEME" ]
                        }
                        funcRange
                  ]
                , context
                )

            else
                ( [], context )

        _ ->
            ( [], context )


isInTcoLocation : Context -> Range -> Bool
isInTcoLocation context range =
    List.member range context.tcoLocations
