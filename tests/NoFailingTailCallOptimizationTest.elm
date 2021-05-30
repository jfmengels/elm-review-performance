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
        , test "should not report an error when a function is properly TCO" <|
            \() ->
                """module A exposing (..)
fun x =
  if condition x then
    fun x - 1
  else
    x
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
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 3 }, end = { row = 3, column = 6 } }
                        ]
        ]
