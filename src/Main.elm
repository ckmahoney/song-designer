module Main exposing (main)

import Html exposing (Html, button, div, text, label, p)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Browser
import Dict
import Debug
import Time

import Types as T
import Data exposing (p1, p2, p3, p4)
import Update as U
import View


type alias Model = 
  T.State T.SynthPreset


init : T.State T.SynthPreset
init =
   { time = 0
   , current = p1
   , presets = []
   }


toInt : Maybe Int ->  Int
toInt x =
  Maybe.withDefault 0 x


view : Model -> Html U.UpdateMsg
view model =
  View.synthEditor model


initFromFlags : Maybe Int -> (Model, Cmd U.UpdateMsg)
initFromFlags ini = 
  let
    w = Debug.log "initializing: " ini
  in
  case ini of 
    _ ->
      (init, U.genPreset)


subs : Model -> Sub U.UpdateMsg
subs model =
  Time.every 1 U.Tick


main =
  Browser.element { init = initFromFlags
                  , update = U.update
                  , view = view 
                  , subscriptions = subs
                  }
