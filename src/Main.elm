module Main exposing (main)

import Html exposing (Html, button, div, text, label, p)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Browser
import Dict

import Types as T
import Data exposing (p1)
import Update as U
import View


type alias Model = 
  T.State T.SynthPreset


init : T.State T.SynthPreset
init =
   { current = p1
   , presets = []
   }


toInt : Maybe Int ->  Int
toInt x =
  Maybe.withDefault 0 x


view : Model -> Html U.UpdateMsg
view model =
  div [] 
    [ div [] [View.synthCard model.current]
    , div [Attr.class "columns"] (List.map View.roleCard Data.roles) ]



main =
  Browser.sandbox { init = init, update = U.update, view = view }
