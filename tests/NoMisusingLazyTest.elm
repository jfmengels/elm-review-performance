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
        [ test "should not report for lazy functions without arguments" <|
            \() ->
                """module A exposing (..)
a =
  Html.Lazy.lazy helper n

helper _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when encountering Html.lazy with arguments and an unstable reference" <|
            \() ->
                """module A exposing (..)
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
        , test "should not report for lazy functions with arguments but stable reference" <|
            \() ->
                """module A exposing (..)
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
        ]
