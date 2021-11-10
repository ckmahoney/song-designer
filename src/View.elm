module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types as T
import Data as D
import Update as U




view : T.State T.SynthPreset -> Html U.UpdateMsg
view model =
    div []
        [ h1 [] [ text "SPA in Elm" ]
        , div []
            [ text "Button pushed in this screen: "

            ]
        , div []
            [ button [  ] [ text "++" ]
            ]
        ]


buttonOpt : T.SynthRole -> msg -> Html msg
buttonOpt role msg =
  button [onClick msg] [text (Tuple.first (D.roleLabel role))]



main =
  Html.text ""
