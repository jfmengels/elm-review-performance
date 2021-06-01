module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Lazy


main : Program () Model Msg
main =
    Browser.sandbox
        { init = 0
        , update = update
        , view = view
        }


type alias Model =
    Int


type Msg
    = Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1


view : Model -> Html Msg
view model =
    Html.div []
        [ header model

        -- Lazy functions
        , workingLazy 1
        , workingLazyWithLambda 1
        , workingLazyWithArgButStableReference 1
        , workingLazyWithLetFunction 1
        , workingLazyUsingIf model 1
        , workingLazyWithPreappliedArguments 1
        , failingLazyWithArgAndUnstableReference 1 2
        , failingLazyWithArgumentAndLetFunction 1 2
        ]


header : Model -> Html Msg
header model =
    Html.div []
        [ Html.h1 [] [ Html.text "Open the console, click on the + button and see what messages are printed" ]
        , Html.div [] [ Html.text "(Ignore the first things to be printed)" ]
        , Html.button [ Events.onClick Increment, Attr.style "width" "400px", Attr.style "height" "400px" ] [ Html.text "+" ]
        , Html.div [] [ Html.text (String.fromInt model) ]
        ]



-- HELPER


viewNothing : String -> a -> Html msg
viewNothing textToPrint _ =
    let
        _ =
            Debug.log textToPrint ()
    in
    Html.text ""



-- LAZY VIEWS


workingLazy : a -> Html msg
workingLazy =
    Html.Lazy.lazy (viewNothing "SHOULD NOT BE PRINTED: workingLazy")


workingLazyWithLambda : a -> Html msg
workingLazyWithLambda =
    Html.Lazy.lazy (\a -> viewNothing "SHOULD NOT BE PRINTED: workingLazyWithLambda" a)


viewNothing_workingLazyWithArgButStableReference : a -> Html msg
viewNothing_workingLazyWithArgButStableReference =
    viewNothing "SHOULD NOT BE PRINTED: workingLazyWithArgButStableReference"


workingLazyWithArgButStableReference : a -> Html msg
workingLazyWithArgButStableReference a =
    Html.Lazy.lazy viewNothing_workingLazyWithArgButStableReference a


viewNothing_ifLazy_Before3 : a -> Html msg
viewNothing_ifLazy_Before3 =
    viewNothing "SHOULD NOT BE PRINTED: workingLazyUsingIf before 3"


viewNothing_ifLazy_After3 : a -> Html msg
viewNothing_ifLazy_After3 =
    viewNothing "SHOULD NOT BE PRINTED: workingLazyUsingIf after 3"


workingLazyUsingIf : Int -> b -> Html msg
workingLazyUsingIf n =
    if n < 3 then
        -- The reference to `viewNothing` is recomputed every time this function is called
        Html.Lazy.lazy viewNothing_ifLazy_Before3

    else
        Html.Lazy.lazy viewNothing_ifLazy_After3


workingLazyWithPreappliedArguments : a -> Html msg
workingLazyWithPreappliedArguments =
    Html.Lazy.lazy2 (\_ -> viewNothing "SHOULD NOT BE PRINTED: workingLazyWithPreappliedArguments") {}


failingLazyWithArgAndUnstableReference : a -> b -> Html msg
failingLazyWithArgAndUnstableReference _ =
    -- The reference to `viewNothing` is recomputed every time this function is called
    Html.Lazy.lazy (viewNothing "failingLazyWithArgAndUnstableReference")


workingLazyWithLetFunction : a -> Html msg
workingLazyWithLetFunction =
    let
        helper : b -> Html msg
        helper =
            viewNothing "SHOULD NOT BE PRINTED: workingLazyWithLetFunction"
    in
    Html.Lazy.lazy helper


failingLazyWithArgumentAndLetFunction : a -> b -> Html msg
failingLazyWithArgumentAndLetFunction _ =
    let
        -- The reference to `helper` is recomputed every time this function is called
        helper : b -> Html msg
        helper =
            viewNothing "failingLazyWithArgumentAndLetFunction"
    in
    Html.Lazy.lazy helper
