module NoFailingTailCallOptimizationTest exposing (all)

import NoFailingTailCallOptimization exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoFailingTailCallOptimization"
        [ test "should not report non-recursive functions" <|
            \() ->
                """module A exposing (..)
a = 1
fun x = a + x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when a function is recursive but applies operations on the result" <|
            \() ->
                """module A exposing (..)
fun x =
  fun x + 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "REPLACEME"
                            }
                        ]
        ]
