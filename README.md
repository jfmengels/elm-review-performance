# elm-review-performance

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to report performance problems.


## Provided rules

- [`NoUnoptimizedRecursion`](https://package.elm-lang.org/packages/jfmengels/elm-review-performance/1.0.2/NoUnoptimizedRecursion/) - Reports recursive functions that are not [tail-call optimized](https://functional-programming-in-elm.netlify.app/recursion/tail-call-elimination.html).


## Configuration

```elm
module ReviewConfig exposing (config)

import NoUnoptimizedRecursion
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-performance/example
```
