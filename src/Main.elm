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


initEditEnsemble : T.SynthState
initEditEnsemble =
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


initEditScore : T.EditScore
initEditScore = 
  { time = 0
  , cps = 1
  , current = Nothing
  , ensembles = Data.allKits
  , layout = Data.layout1
  , list = Data.score1
  } 


toInt : Maybe Int ->  Int
toInt x =
  Maybe.withDefault 0 x


viewEnsembleEditor : T.SynthState -> Html U.UpdateMsg
viewEnsembleEditor model =
  div [Attr.class "syn-main section"] 
    [ View.editEnsemble model ]


viewLayoutEditor : T.EditLayout -> Html U.EditLayout
viewLayoutEditor model =
  div [Attr.class "syn-main section"] 
    [ View.editLayout model ]


viewScoreEditor : T.EditScore -> Html U.EditScore
viewScoreEditor model =
  div [Attr.class "syn-main section"] 
    [ View.editScore model ]


initEnsemble : Maybe Int -> (T.SynthState, Cmd U.UpdateMsg)
initEnsemble ini = 
  let
    w = Debug.log "initializing layout: " ini
  in
  case ini of 
    _ ->
      (initEditEnsemble, Cmd.none)


initLayout : Maybe Int -> (T.EditLayout, Cmd U.EditLayout)
initLayout ini = 
  let
    w = Debug.log "initializing layout: " ini
  in
  case ini of 
    _ ->
      (initEditLayout, Cmd.none)


initScore : Maybe Int -> (T.EditScore, Cmd U.EditScore)
initScore ini = 
  let
    w = Debug.log "initializing score: " ini
  in
  case ini of 
    _ ->
      (initEditScore, Cmd.none)


subsEnsemble : T.SynthState -> Sub U.UpdateMsg
subsEnsemble model =
  Time.every 1000 U.Tick


subsLayout : T.EditLayout -> Sub U.EditLayout
subsLayout model =
  Sub.none


subsScore model =
  Sub.none


mainEnsembleEditor =
  Browser.element { init = initEnsemble
                  , update = U.updateEnsemble
                  , view = viewEnsembleEditor
                  , subscriptions = subsEnsemble
                  }


mainLayoutEditor =
  Browser.element { init = initLayout
                  , update = U.updateLayout
                  , view = viewLayoutEditor
                  , subscriptions = subsLayout
                  }


mainScoreEditor =
  Browser.element { init = initScore
                  , update = U.updateScore
                  , view = viewScoreEditor
                  , subscriptions = subsScore
                  }

main = 
  mainEnsembleEditor
