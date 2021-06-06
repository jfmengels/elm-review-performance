module NoMisusingLazyTest exposing (all)

import Expect exposing (Expectation)
import NoMisusingLazy exposing (defaults, rule, withLazyModules)
import Review.Project
import Review.Rule exposing (Rule)
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
        , configurationTests
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
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should not report for lazy function without arguments and unstable reference" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a =
  Html.Lazy.lazy helper n

helper _ = text ""
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should not report for lazy function without arguments and lambda argument" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a =
  Html.Lazy.lazy (\\_ -> helper) n

helper _ = text ""
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runOnModulesWithProjectData project (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should report an error when encountering Html.lazy with arguments and an unstable reference" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a n =
  Html.Lazy.lazy (helper x) n

helper _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Html.Lazy.lazy"
                            }
                        ]
        , test "should report an error when encountering Html.lazy with an unstable reference passed using <|" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
a n =
  Html.Lazy.lazy <| helper x

helper _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should not report an error when lazy is not from one of the known supported libraries" <|
            \() ->
                """module A exposing (..)
a n =
  lazy helper n
lazy _ _ = text ""
helper _ = text ""
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "VirtualDom.lazy"
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
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
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
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Element.WithContext.Lazy.lazy"
                            }
                        ]
        ]


configurationTests : Test
configurationTests =
    describe "Configuration"
        [ test "should report an error when encountering problematic custom lazy function" <|
            \() ->
                let
                    configuredRule : Rule
                    configuredRule =
                        defaults
                            |> withLazyModules [ "Some.Module" ]
                            |> rule
                in
                """module A exposing (..)
import Some.Module
a n =
  Some.Module.lazy (helper x)
helper _ _ _ = text ""
"""
                    |> Review.Test.runWithProjectData project configuredRule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "Some.Module.lazy"
                            }
                        ]
        , test "should report a configuration error when passed module name is invalid" <|
            \() ->
                defaults
                    |> withLazyModules [ "Some.module" ]
                    |> rule
                    |> Review.Test.expectConfigurationError
                        { message = "I found some problems with the arguments to withLazyModules"
                        , details = [ "  - Some.module is not a valid module name" ]
                        }
        , test "should report a configuration error when passed an empty module name" <|
            \() ->
                defaults
                    |> withLazyModules [ "" ]
                    |> rule
                    |> Review.Test.expectConfigurationError
                        { message = "I found some problems with the arguments to withLazyModules"
                        , details = [ "  - One of the module names I received was empty" ]
                        }
        ]


argumentReferenceTests : Test
argumentReferenceTests =
    describe "Arguments passed to lazy functions"
        [ test "should not report errors when argument to lazy function is a function or value" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView =
    Html.Lazy.lazy helper
helper _ = text ""

view model =
    lazyView model
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should not report errors when argument to lazy function is stable but wrapped in parens" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView =
    Html.Lazy.lazy (helper)
helper _ = text ""

view model =
    lazyView model
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should not report errors when argument to lazy function is an unknown function call" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView =
    Html.Lazy.lazy (helper)
helper _ = text ""

view model =
    lazyView (unknown argument)
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should not report errors when argument to lazy function is an unknown function call (<|)" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView =
    Html.Lazy.lazy (helper)
helper _ = text ""

view model =
    lazyView (unknown <| argument)
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should not report errors when argument to lazy function is an unknown function call (|>)" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView =
    Html.Lazy.lazy (helper)
helper _ = text ""

view model =
    lazyView (argument |> unknown)
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should not report errors when argument is an unstable references but the function takes no arguments" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView =
    Html.Lazy.lazy helper
helper _ = text ""

view =
    lazyView {}
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should not report errors when argument to lazy function is a type alias" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView =
    Html.Lazy.lazy2 (helper)
helper _ = text ""

type alias RecordAlias = {}

view model =
    lazyView RecordAlias
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should not report errors when argument to lazy function is a custom type constructor" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView =
    Html.Lazy.lazy2 (helper)
helper _ = text ""

type CustomType = Constructor

view model =
    lazyView Constructor
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectNoErrors
        , test "should report errors when argument to lazy function is a record literal inside parens" <|
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
        , test "should report errors when arguments to lazy function is function call of a type or type alias" <|
            \() -> reportWhenArgumentIs "(Constructor argument)"
        , test "should report errors when arguments to lazy function is function call of a type or type alias (using <|)" <|
            \() -> reportWhenArgumentIs "(Constructor <| argument)"
        , test "should report errors when arguments to lazy function is function call of a type or type alias (using |>)" <|
            \() -> reportWhenArgumentIs "(argument |> Constructor)"
        , test "should report errors when arguments to lazy function is function call of a type or type alias with already an argument (using <|)" <|
            \() -> reportWhenArgumentIs "(Constructor 1 <| argument)"
        , test "should report errors when arguments to lazy function is function call of a type or type alias with already an argument (using |>)" <|
            \() -> reportWhenArgumentIs "(argument |> Constructor 1)"
        , test "should report errors when arguments to lazy function is function call of a type or type alias with already an argument (using <| and extra parens)" <|
            \() -> reportWhenArgumentIs "((Constructor 1) <| argument)"
        , test "should report errors when arguments to function sprinkled with lazy are unstable" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView n =
    Html.Lazy.lazy helper {}
helper = text
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "FOO"
                            , details = [ "BAR" ]
                            , under = "Html.Lazy.lazy"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 19 } }
                        ]
        , test "should report errors when arguments to function sprinkled with lazy (lazy wrapped in parens) are unstable" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView n =
    (Html.Lazy.lazy) helper {}
helper = text
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "FOO"
                            , details = [ "BAR" ]
                            , under = "Html.Lazy.lazy"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 6 }, end = { row = 4, column = 20 } }
                        ]
        , test "should report errors when arguments to function sprinkled with lazy (function wrapped in parens) are unstable" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView n =
    Html.Lazy.lazy (helper) {}
helper = text
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "FOO"
                            , details = [ "BAR" ]
                            , under = "Html.Lazy.lazy"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 19 } }
                        ]
        , test "should report errors when arguments to function sprinkled with lazy are unstable (using <|)" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView n =
    Html.Lazy.lazy helper <| {}
helper = text
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "FOO"
                            , details = [ "BAR" ]
                            , under = "Html.Lazy.lazy"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 19 } }
                        ]
        , test "should report errors when arguments to function sprinkled with lazy are unstable (using |>)" <|
            \() ->
                """module A exposing (..)
import Html.Lazy
lazyView n =
    {} |> Html.Lazy.lazy helper
helper = text
"""
                    |> Review.Test.runWithProjectData project (rule defaults)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "FOO"
                            , details = [ "BAR" ]
                            , under = "Html.Lazy.lazy"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 11 }, end = { row = 4, column = 25 } }
                        ]
        , Test.skip <|
            test "should report errors when arguments to function sprinkled with lazy and wrapped in parens are unstable" <|
                \() ->
                    """module A exposing (..)
import Html.Lazy
lazyView n =
    (Html.Lazy.lazy helper) {}
helper = text
"""
                        |> Review.Test.runWithProjectData project (rule defaults)
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "FOO"
                                , details = [ "BAR" ]
                                , under = "Html.Lazy.lazy"
                                }
                                |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 19 } }
                            ]
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
        |> Review.Test.runWithProjectData project (rule defaults)
        |> Review.Test.expectErrors
            [ Review.Test.error
                { message = "FOO"
                , details = [ "BAR" ]
                , under = "lazyView"
                }
                |> Review.Test.atExactly { start = { row = 8, column = 5 }, end = { row = 8, column = 13 } }
            ]
