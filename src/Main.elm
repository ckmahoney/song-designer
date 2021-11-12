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
  T.EditScore


initEditSynth : T.SynthState
initEditSynth =
   { time = 0
   , current = Just p1
   , presets = Data.kitAll
   }

initEditScore : T.EditScore
initEditScore =
   { time = 0
   , index = -1
   , current = Nothing
   , presets = Data.layout1
   }


toInt : Maybe Int ->  Int
toInt x =
  Maybe.withDefault 0 x


view : Model -> Html U.EditScore
view model =
  div [Attr.class "syn-main section"] 
    [ View.editScore model ]


-- initFromFlags : Maybe Int -> (Model, Cmd U.UpdateMsg)
-- initFromFlags ini = 
--   let
--     w = Debug.log "initializing: " ini
--   in
--   case ini of 
--     _ ->
--       (initEditSynth, Cmd.none)

initScore : Maybe Int -> (Model, Cmd U.EditScore)
initScore ini = 
  let
    w = Debug.log "initializing score: " ini
  in
  case ini of 
    _ ->
      (initEditScore, Cmd.none)


subs : Model -> Sub U.UpdateMsg
subs model =
  Time.every 1000 U.Tick

subsScore : Model -> Sub U.EditScore
subsScore model =
  Sub.none


main =
  Browser.element { init = initScore
                  , update = U.updateScore
                  , view = view 
                  , subscriptions = subsScore
                  }
