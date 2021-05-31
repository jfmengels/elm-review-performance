# elm-review-performance

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.


## Provided rules

- [`NoUnoptimizedRecursion`](https://package.elm-lang.org/packages/jfmengels/elm-review-performance/1.0.0/NoUnoptimizedRecursion) - Reports REPLACEME.


## Configuration

```elm
module ReviewConfig exposing (config)

import NoUnoptimizedRecursion
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoUnoptimizedRecursion.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-performance/example
```
