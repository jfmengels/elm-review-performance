module NoFailingTailCallOptimizationTest exposing (all)

import NoFailingTailCallOptimization exposing (optInWithComment, optOutWithComment, rule)
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
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should report an error when a function is recursive but applies operations on the result" <|
            \() ->
                """module A exposing (..)
fun x =
  fun x + 1
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Recursive function is not tail-call optimized"
                            , details = [ "The way this function is called recursively here prevents the function from being tail-call optimized." ]
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
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
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
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
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
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Recursive function is not tail-call optimized"
                            , details = [ "The way this function is called recursively here prevents the function from being tail-call optimized." ]
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
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Recursive function is not tail-call optimized"
                            , details = [ "The way this function is called recursively here prevents the function from being tail-call optimized." ]
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
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
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
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should report an error when a function is called recursively from inside one of its let functions" <|
            \() ->
                """module A exposing (..)
fun x =
  let
    fun2 y =
      fun x
  in
  fun2 x
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Recursive function is not tail-call optimized"
                            , details = [ "The way this function is called recursively here prevents the function from being tail-call optimized." ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 7 }, end = { row = 5, column = 10 } }
                        ]
        , test "should not report an error when a function is properly TCO (parentheses)" <|
            \() ->
                """module A exposing (..)
fun x =
  (fun (x - 1))
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should report an error when a function calls itself in a lambda" <|
            \() ->
                """module A exposing (..)
fun n =
    \\x -> (fun n x)
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Recursive function is not tail-call optimized"
                            , details = [ "The way this function is called recursively here prevents the function from being tail-call optimized." ]
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
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Recursive function is not tail-call optimized"
                            , details = [ "The way this function is called recursively here prevents the function from being tail-call optimized." ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 8, column = 16 }, end = { row = 8, column = 19 } }
                        ]
        , test "should report an error when a function calls itself using |> (simple reference)" <|
            \() ->
                """module A exposing (..)
fun x =
    n
        |> fun
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Recursive function is not tail-call optimized"
                            , details = [ "The way this function is called recursively here prevents the function from being tail-call optimized." ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 12 }, end = { row = 4, column = 15 } }
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
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Recursive function is not tail-call optimized"
                            , details = [ "The way this function is called recursively here prevents the function from being tail-call optimized." ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 7, column = 9 }, end = { row = 7, column = 12 } }
                        ]
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
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Recursive function is not tail-call optimized"
                            , details = [ "The way this function is called recursively here prevents the function from being tail-call optimized." ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 7 }, end = { row = 5, column = 10 } }
                        ]
        , test "should not report an error for TCO let functions" <|
            \() ->
                """module A exposing (..)
a n =
  let
    fun x =
      fun x
  in
  fun 2
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should not report an error for TCO let functions inside a recursive function" <|
            \() ->
                """module A exposing (..)
fun1 n =
  let
    fun2 x =
      fun2 x
  in
  if cond then
    fun1 n
  else
    fun2 2
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the function body contains has the exact opt out comment" <|
            \() ->
                """module A exposing (..)
fun x =
  -- OPT OUT
  fun x + 1
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the function body contains the opt out comment" <|
            \() ->
                """module A exposing (..)
fun x =
  -- SOME TAG, OPT OUT, OTHER TAG
  fun x + 1
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should not report an error when the function body contains the opt out comment when it has a signature" <|
            \() ->
                """module A exposing (..)
fun : Int -> Int
fun x =
  -- OPT OUT
  fun x + 1
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should not report an error for let functions when the function body contains the opt out comment" <|
            \() ->
                """module A exposing (..)
a n =
  let
    fun x =
      -- OPT OUT
      fun x + 1
  in
  fun x
"""
                    |> Review.Test.run (rule (optOutWithComment "OPT OUT"))
                    |> Review.Test.expectNoErrors
        , test "should report an error when the function body contains the opt in comment" <|
            \() ->
                """module A exposing (..)
fun x =
  -- OPT IN
  fun x + 1
"""
                    |> Review.Test.run (rule (optInWithComment "OPT IN"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Recursive function is not tail-call optimized"
                            , details = [ "The way this function is called recursively here prevents the function from being tail-call optimized." ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 3 }, end = { row = 4, column = 6 } }
                        ]
        , test "should report an error for let functions when the function body contains the opt in comment" <|
            \() ->
                """module A exposing (..)
a n =
  let
    fun x =
      -- OPT IN
      fun x + 1
  in
  fun x
"""
                    |> Review.Test.run (rule (optInWithComment "OPT IN"))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Recursive function is not tail-call optimized"
                            , details = [ "The way this function is called recursively here prevents the function from being tail-call optimized." ]
                            , under = "fun"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 7 }, end = { row = 6, column = 10 } }
                        ]
        , test "should not report an error when the function body does not contain the opt in comment" <|
            \() ->
                """module A exposing (..)
fun x =
  fun x + 1
"""
                    |> Review.Test.run (rule (optInWithComment "OPT IN"))
                    |> Review.Test.expectNoErrors
        ]
