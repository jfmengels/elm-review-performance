module NoPartialApplicationTest exposing (all)

import NoPartialApplication exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoPartialApplication"
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
                            { message = "Function was partially applied"
                            , details = [ "REPLACEME" ]
                            , under = "function"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 13 } }
                        ]
        , test "should not report an error when calling a local function with the expected number of arguments" <|
            \() ->
                """module A exposing (..)
a = function 0 1
function b c = b + c
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when calling a local function with more than the expected number of arguments" <|
            \() ->
                """module A exposing (..)
a = function 0 1 2
function b c = something b c
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when calling a local function with the correct arity using |>" <|
            \() ->
                """module A exposing (..)
a = 1 |> function 0
function b c = b + c
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when calling a local function with insufficient arity using |> (simple reference)" <|
            \() ->
                """module A exposing (..)
a = 1 |> function
function b c = b + c
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Function was partially applied"
                            , details = [ "REPLACEME" ]
                            , under = "function"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 10 }, end = { row = 2, column = 18 } }
                        ]
        , test "should report an error when calling a local function with insufficient arity using |> (application)" <|
            \() ->
                """module A exposing (..)
a = 1 |> function 0
function b c d = b + c + d
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Function was partially applied"
                            , details = [ "REPLACEME" ]
                            , under = "function"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 10 }, end = { row = 2, column = 18 } }
                        ]
        , test "should not report an error when calling a local function with the correct arity using <|" <|
            \() ->
                """module A exposing (..)
a = function 0 <| 1
function b c = b + c
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when calling a local function with insufficient arity using <| (simple reference)" <|
            \() ->
                """module A exposing (..)
a = function <| 1
function b c = b + c
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Function was partially applied"
                            , details = [ "REPLACEME" ]
                            , under = "function"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 13 } }
                        ]
        , test "should report an error when calling a local function with insufficient arity using <| (application)" <|
            \() ->
                """module A exposing (..)
a = function 0 <| 1
function b c d = b + c + d
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Function was partially applied"
                            , details = [ "REPLACEME" ]
                            , under = "function"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 13 } }
                        ]
        , test "should report an error when calling a other module's function with insufficient arity" <|
            \() ->
                [ """module A exposing (..)
import B
a = B.function 0
""", """module B exposing (function)
function b c = b + c
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Function was partially applied"
                                , details = [ "REPLACEME" ]
                                , under = "B.function"
                                }
                            ]
                          )
                        ]
        ]
