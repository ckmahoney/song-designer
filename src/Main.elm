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
  T.EditLayout


initEditSynth : T.SynthState
initEditSynth =
   { time = 0
   , current = Just p1
   , presets = Data.kitAll
   }

initEditLayout : T.EditLayout
initEditLayout =
   { time = 0
   , index = -1
   , current = Nothing
   , presets = Data.layout1
   }


toInt : Maybe Int ->  Int
toInt x =
  Maybe.withDefault 0 x


view : Model -> Html U.EditLayout
view model =
  div [Attr.class "syn-main section"] 
    [ View.editLayout model ]


-- initFromFlags : Maybe Int -> (Model, Cmd U.UpdateMsg)
-- initFromFlags ini = 
--   let
--     w = Debug.log "initializing: " ini
--   in
--   case ini of 
--     _ ->
--       (initEditSynth, Cmd.none)

initLayout : Maybe Int -> (Model, Cmd U.EditLayout)
initLayout ini = 
  let
    w = Debug.log "initializing score: " ini
  in
  case ini of 
    _ ->
      (initEditLayout, Cmd.none)


subs : Model -> Sub U.UpdateMsg
subs model =
  Time.every 1000 U.Tick

subsLayout : Model -> Sub U.EditLayout
subsLayout model =
  Sub.none


main =
  Browser.element { init = initLayout
                  , update = U.updateLayout
                  , view = view 
                  , subscriptions = subsLayout
                  }
