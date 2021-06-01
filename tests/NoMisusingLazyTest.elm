module NoMisusingLazyTest exposing (all)

import NoMisusingLazy exposing (rule)
import Review.Project
import Review.Test
import Review.Test.Dependencies
import Test exposing (Test, describe, test)


project : Review.Project.Project
project =
    Review.Test.Dependencies.projectWithElmCore
        |> Review.Project.addDependency Review.Test.Dependencies.elmHtml


all : Test
all =
    describe "NoMisusingLazy"
        [ test "should not report for lazy function without arguments and stable reference" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a =
  Html.Lazy.lazy helper n

helper _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should not report for lazy function without arguments and unstable reference" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a =
  Html.Lazy.lazy helper n

helper _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should not report for lazy function without arguments and lambda argument" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a =
  Html.Lazy.lazy (\\_ -> helper) n

helper _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should not report for lazy function without arguments and let in argument" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a =
  let
    helper _ = text ""
  in
  Html.Lazy.lazy helper n

"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should not report for lazy function with arguments and stable reference to other module" <|
            \() ->
                [ """module A exposing (..)
import Html.Lazy
import B
a n =
  Html.Lazy.lazy B.helper n
""", """module B exposing (..)
helper _ = text ""
""" ]
                    |> Review.Test.runOnModulesWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when encountering Html.lazy with arguments and an unstable reference" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a n =
  Html.Lazy.lazy (helper x) n

helper _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Misuse of a lazy function"
                            , details = [ "REPLACEME" ]
                            , under = "Html.Lazy.lazy"
                            }
                        ]
        , test "should not report for lazy function with arguments but stable reference" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a n =
  Html.Lazy.lazy helper n

helper _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should not report an error when lazy is not from one of the known supported libraries" <|
            \() ->
                """module A exposing (..)
a n =
  lazy helper n

lazy _ _ = text ""

helper _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when encountering Html.lazy with arguments and an let in reference" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a n =
  let
    helper _ _ = text ""
  in
  Html.Lazy.lazy helper n

"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Misuse of a lazy function"
                            , details = [ "REPLACEME" ]
                            , under = "Html.Lazy.lazy"
                            }
                        ]
        ]
