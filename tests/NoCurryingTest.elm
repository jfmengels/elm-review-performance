module NoCurryingTest exposing (all)

import NoCurrying exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoCurrying"
        [ test "should not report an error when using functions with all the correct arguments" <|
            \() ->
                """module A exposing (..)
a = List.map function list
function n = n
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when calling a local function with less than the expected number of arguments" <|
            \() ->
                """module A exposing (..)
a = function 0
function b c = b + c
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "function"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 13 } }
                        ]
        ]
