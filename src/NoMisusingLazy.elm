module NoMisusingLazy exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Rule as Rule exposing (Rule)


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
    Rule.newModuleRuleSchema "NoMisusingLazy" ()
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    ()


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.Application ((Node functionRange (Expression.FunctionOrValue _ "lazy")) :: _) ->
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
