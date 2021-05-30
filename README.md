# elm-review-performance

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.


## Provided rules

- [`NoFailingTailCallOptimization`](https://package.elm-lang.org/packages/jfmengels/elm-review-performance/1.0.0/NoFailingTailCallOptimization) - Reports REPLACEME.


## Configuration

```elm
module ReviewConfig exposing (config)

import NoFailingTailCallOptimization
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoFailingTailCallOptimization.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-performance/example
```
