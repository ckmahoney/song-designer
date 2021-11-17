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


initEditLayout : T.EditLayout
initEditLayout =
   { time = 0
   , index = -1
   , current = Nothing
   , list = []
   , presets = Data.layout1
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
    [ View.designLayout model ]


viewScoreEditor : T.EditScore -> Html U.EditScore
viewScoreEditor model =
  div [Attr.class "syn-main section"] 
    -- [ View.editScore model ]
    [ View.overviewScore model.list ]



initEnsemble : Maybe Int -> (T.SynthState, Cmd U.UpdateMsg)
initEnsemble ini = 
  let
    w = Debug.log "initializing layout: " ini
  in
  case ini of 
    _ ->
      (Data.initEditEnsemble, Cmd.none)


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
      (Data.initEditScore, Cmd.none)


-- initMill : Maybe Int -> (Mill.Model User, Cmd (Mill.Msg User))
-- initMill ini = 
--   let
--     w = Debug.log "initializing score: " ini
--   in
--   case ini of 
--     _ ->
--       (Mill.initWith joe, Cmd.none)


initApp : Maybe Int -> (App.Module, Cmd msg)
initApp = 
   App.init

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


-- mainMill =
--   Browser.element { init = initMill
--                   , update = Mill.update
--                   , view = Mill.view
--                   , subscriptions = Mill.subscriptions
--                   }


main = 
  -- mainSongDesigner
  mainEnsembleEditor 
  -- mainLayoutEditor 
  -- mainScoreEditor 
  -- mainRouter
