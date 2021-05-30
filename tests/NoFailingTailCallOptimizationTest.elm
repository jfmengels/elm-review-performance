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
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 3 }, end = { row = 3, column = 6 } }
                        ]
        , test "should not report an error when a function is properly TCO (if then branch)" <|
            \() ->
                """module A exposing (..)
fun x =
  if condition x then
    fun (x - 1)
  else
    x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when a function is properly TCO (if else branch)" <|
            \() ->
                """module A exposing (..)
fun x =
  if condition x then
    x
  else
    fun (x - 1)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when a function calls itself in the condition of an if block" <|
            \() ->
                """module A exposing (..)
fun x =
  if fun (x - 1) then
    1
  else
    x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 6 }, end = { row = 3, column = 9 } }
                        ]
        , test "should report an error when a function calls itself in the case of pattern to evaluate" <|
            \() ->
                """module A exposing (..)
fun x =
  case fun (x - 1) of
    _ -> 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 8 }, end = { row = 3, column = 11 } }
                        ]
        , test "should not report an error when a function is properly TCO (case branch)" <|
            \() ->
                """module A exposing (..)
fun x =
  case x of
    _ -> fun (x - 1)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when a function is properly TCO (let body)" <|
            \() ->
                """module A exposing (..)
fun x =
  let
    y = x - 1
  in
  fun y
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when a function is called recursively from inside one of its let functions" <|
            \() ->
                """module A exposing (..)
fun x =
  let
    fun2 y = fun x
  in
  fun2 x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 14 }, end = { row = 4, column = 17 } }
                        ]
        , test "should not report an error when a function is properly TCO (parentheses)" <|
            \() ->
                """module A exposing (..)
fun x =
  (fun (x - 1))
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when a function calls itself in a lambda" <|
            \() ->
                """module A exposing (..)
fun n =
    \\x -> (fun n x)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 12 }, end = { row = 3, column = 15 } }
                        ]
        , test "should report an error when a function calls itself using |>" <|
            \() ->
                """module A exposing (..)
fun x n =
    if x <= 0 then
        n

    else
        n
            |> fun (x - 1)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 8, column = 16 }, end = { row = 8, column = 19 } }
                        ]
        , test "should report an error when a function calls itself using <|" <|
            \() ->
                """module A exposing (..)
fun x n =
    if x <= 0 then
        n

    else
        fun (x - 1) <| n
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 9 }, end = { row = 7, column = 12 } }
                        ]
        , test "should not report an error when a function is referencing but not calling itself" <|
            -- TODO Check that this doesn't actually invalidate TCO
            \() ->
                """module A exposing (..)
fun x =
  if condition x then
    \\_ -> fun
  else
    x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error for non-TCO let functions" <|
            \() ->
                """module A exposing (..)
a n =
  let
    fun x =
      fun x + 1
  in
  fun 2
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
