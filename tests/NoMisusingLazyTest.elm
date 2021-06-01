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
        [ test "should report an error when encountering Html.lazy with arguments and an unstable reference" <|
            \() ->
                """module A exposing (..)
a n =
  Html.Lazy.lazy helper n

helper _ = text ""
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
