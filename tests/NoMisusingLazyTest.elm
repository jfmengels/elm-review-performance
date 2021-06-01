module NoMisusingLazyTest exposing (all)

import Expect exposing (Expectation)
import NoMisusingLazy exposing (rule)
import Review.Project
import Review.Test
import Review.Test.Dependencies
import Test exposing (Test, describe, test)


message : String
message =
    "Misuse of a lazy function"


details : List String
details =
    [ "The argument passed to the lazy function must be a stable reference, but a new reference will be created everytime this function is called." ]


project : Review.Project.Project
project =
    Review.Test.Dependencies.projectWithElmCore
        |> Review.Project.addDependency Review.Test.Dependencies.elmHtml


all : Test
all =
    describe "NoMisusingLazy"
        [ lazyDefinitionTests
        , argumentReferenceTests
        ]


lazyDefinitionTests : Test
lazyDefinitionTests =
    describe "Lazy definitions"
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
                            { message = message
                            , details = details
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
                            { message = message
                            , details = details
                            , under = "Html.Lazy.lazy"
                            }
                        ]
        , test "should report an error when encountering problematic Html.Lazy.lazy2" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a n =
  Html.Lazy.lazy2 (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Html.Lazy.lazy2"
                            }
                        ]
        , test "should report an error when encountering problematic Html.Lazy.lazy3" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a n =
  Html.Lazy.lazy3 (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Html.Lazy.lazy3"
                            }
                        ]
        , test "should report an error when encountering problematic Html.Lazy.lazy4" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a n =
  Html.Lazy.lazy4 (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Html.Lazy.lazy4"
                            }
                        ]
        , test "should report an error when encountering problematic Html.Lazy.lazy5" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a n =
  Html.Lazy.lazy5 (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Html.Lazy.lazy5"
                            }
                        ]
        , test "should report an error when encountering problematic Html.Lazy.lazy6" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a n =
  Html.Lazy.lazy6 (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Html.Lazy.lazy6"
                            }
                        ]
        , test "should report an error when encountering problematic Html.Lazy.lazy7" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a n =
  Html.Lazy.lazy7 (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Html.Lazy.lazy7"
                            }
                        ]
        , test "should report an error when encountering problematic Html.Lazy.lazy8" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a n =
  Html.Lazy.lazy8 (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Html.Lazy.lazy8"
                            }
                        ]
        , test "should report an error when encountering problematic Svg.Lazy.lazy" <|
            \() ->
                """module A exposing (..)
import Svg.Lazy
a n =
  Svg.Lazy.lazy (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Svg.Lazy.lazy"
                            }
                        ]
        , test "should report an error when encountering problematic VirtualDom.lazy" <|
            \() ->
                """module A exposing (..)
import VirtualDom
a n =
  VirtualDom.lazy (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "VirtualDom.lazy"
                            }
                        ]
        , test "should report an error when encountering problematic Html.String.Lazy.lazy" <|
            \() ->
                """module A exposing (..)
import VirtualDom
a n =
  Html.String.Lazy.lazy (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Html.String.Lazy.lazy"
                            }
                        ]
        , test "should report an error when encountering problematic Html.Styled.Lazy.lazy" <|
            \() ->
                """module A exposing (..)
import VirtualDom
a n =
  Html.Styled.Lazy.lazy (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Html.Styled.Lazy.lazy"
                            }
                        ]
        , test "should report an error when encountering problematic Svg.Styled.Lazy.lazy" <|
            \() ->
                """module A exposing (..)
import VirtualDom
a n =
  Svg.Styled.Lazy.lazy (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Svg.Styled.Lazy.lazy"
                            }
                        ]
        , test "should report an error when encountering problematic Element.Lazy.lazy" <|
            \() ->
                """module A exposing (..)
import VirtualDom
a n =
  Element.Lazy.lazy (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Element.Lazy.lazy"
                            }
                        ]
        , test "should report an error when encountering problematic Element.WithContext.Lazy.lazy" <|
            \() ->
                """module A exposing (..)
import VirtualDom
a n =
  Element.WithContext.Lazy.lazy (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Element.WithContext.Lazy.lazy"
                            }
                        ]
        ]


argumentReferenceTests : Test
argumentReferenceTests =
    describe "Arguments passed to lazy functions"
        [ test "should not report errors when arguments to lazy function is a function or value" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView =
    Html.Lazy.lazy helper
helper _ = text ""

view model =
    lazyView model
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should not report errors when arguments are unstable references but the function takes no arguments" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView =
    Html.Lazy.lazy helper
helper _ = text ""

view =
    lazyView {}
"""
                    |> Review.Test.runWithProjectData project rule
                    |> Review.Test.expectNoErrors
        , test "should report errors when arguments to lazy function is a record literal inside parens" <|
            \() -> reportWhenArgumentIs "({})"
        , test "should report errors when arguments to lazy function is a record update function" <|
            \() -> reportWhenArgumentIs "{ model | a = 1 }"
        , test "should report errors when arguments to lazy function is a tuple" <|
            \() -> reportWhenArgumentIs "( 1, 2 )"
        , test "should report errors when arguments to lazy function is a list literal" <|
            \() -> reportWhenArgumentIs "[]"
        , test "should report errors when arguments to lazy function is a record access function" <|
            \() -> reportWhenArgumentIs ".name"
        , test "should report errors when arguments to lazy function is a lambda" <|
            \() -> reportWhenArgumentIs "(\\a -> a.name)"
        ]


reportWhenArgumentIs : String -> Expectation
reportWhenArgumentIs thing =
    ("""module A exposing (..)
import Html.Lazy
lazyView =
    Html.Lazy.lazy helper
helper _ = text ""

view model =
    lazyView """ ++ thing)
        |> Review.Test.runWithProjectData project rule
        |> Review.Test.expectErrors
            [ Review.Test.error
                { message = "FOO"
                , details = [ "BAR" ]
                , under = "lazyView"
                }
                |> Review.Test.atExactly { start = { row = 8, column = 5 }, end = { row = 8, column = 13 } }
            ]
