module Main exposing (main)


import Html exposing (Html, button, div, text, label, p)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Browser
import Dict
import Debug
import Time


import App
import Types as T
import Data exposing (p1, p2, p3, p4)
import Update as U
import View 
import Router 


type alias User = 
  { username : String
  , token : Bool
  }


joe = 
  { username = "Joe"
  , token = True
  }


toInt : Maybe Int ->  Int
toInt x =
  Maybe.withDefault 0 x


initEnsemble : Maybe Int -> (T.VoiceEditor, Cmd U.UpdateMsg)
initEnsemble ini = 
  case ini of 
    _ ->
      (Data.initEditEnsemble, Cmd.none)


initVoiceEditor : Maybe Int -> (T.VoiceEditor, Cmd U.EditVoice)
initVoiceEditor ini =
  case ini of 
    _ ->
     (Data.initVoiceEditor, Cmd.none)


initEnsembleEditor : Maybe Int -> (T.EnsembleEditor, Cmd U.EditEnsemble)
initEnsembleEditor ini = 
  case ini of 
    _ ->
      (Data.initEnsembleEditor, Cmd.none)


initScore : Maybe Int -> (T.EditScore, Cmd U.EditScore)
initScore ini = 
  let
    w = Debug.log "initializing score: " ini
  in
  case ini of 
    _ ->
      (Data.initEditScore, Cmd.none)


initApp : Maybe Int -> (App.Module, Cmd msg)
initApp = 
   App.init


subsEnsemble : a -> Sub U.UpdateMsg
subsEnsemble model =
  Time.every 1000 U.Tick


subsEnsembleEditor : T.EnsembleEditor -> Sub U.EditEnsemble
subsEnsembleEditor model =
  Sub.none


subsLayout : T.EditLayout -> Sub U.EditLayout
subsLayout model =
  Sub.none


subsScore model =
  Sub.none


subsVoice : T.VoiceEditor -> Sub U.EditVoice
subsVoice model =
  Sub.none


mainSongDesigner =
  Browser.element { init = App.init
                  , update = App.update
                  , view = App.view
                  , subscriptions = App.subscriptions
                  }


mainRouter =
  Browser.element { init = Router.init
                  , update = Router.update
                  , view = Router.view
                  , subscriptions = Router.subscriptions
                  }


main = 
  -- mainSongDesigner
  -- mainVoiceEditor
  -- mainLayoutEditor 
  mainRouter
