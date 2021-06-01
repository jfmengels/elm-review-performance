module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events as Events


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
        [ Html.button [ Events.onClick Increment ] [ Html.text "+" ]
        , Html.div [] [ Html.text (String.fromInt model) ]
        ]
