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
        [ Html.h1 [] [ Html.text "Open the console, click on the + button and see what messages are printed" ]
        , Html.div [] [ Html.text "(Ignore the first things to be printed)" ]
        , Html.button [ Events.onClick Increment, Attr.style "width" "400px", Attr.style "height" "400px" ] [ Html.text "+" ]
        , Html.div [] [ Html.text (String.fromInt model) ]
        , workingLazy 1
        , workingLazyWithLambda 1
        , failingLazy 1 2
        ]


viewNothing : String -> a -> Html msg
viewNothing textToPrint _ =
    let
        _ =
            Debug.log textToPrint ()
    in
    Html.text ""


workingLazy : a -> Html msg
workingLazy =
    Html.Lazy.lazy (viewNothing "SHOULD NOT BE PRINTED: workingLazy")


workingLazyWithLambda : a -> Html msg
workingLazyWithLambda =
    Html.Lazy.lazy (\a -> viewNothing "SHOULD NOT BE PRINTED: workingLazyWithLambda" a)


failingLazy : a -> b -> Html msg
failingLazy a =
    Html.Lazy.lazy (viewNothing "failingLazy")
