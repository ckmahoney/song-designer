module View exposing (main, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (Model)
import Update


view : Model -> Html Update.Msg
view model =
    div []
        [ h1 [] [ text "SPA in Elm" ]
        , div []
            [ text "Button pushed in this screen: "
            , text <| String.fromInt  model.counter
            ]
        , div []
            [ button [ onClick Update.ButtonClick ] [ text "++" ]
            ]
        ]

main =
  Html.text ""
