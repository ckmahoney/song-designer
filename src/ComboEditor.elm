module ComboEditor exposing (..)

import Browser
import Html exposing (Html, button, div, text, label, p, input)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)

import Types exposing (..)
import View
import Elements
import Tools
import Components
import Data
import ScopeEditor
import EnsembleEditor
import VoiceEditor


type alias State = Combo


type Msg
  = Save State
  | UpdateScopeEditor ScopeEditor.Msg
  | UpdateEnsembleEditor EnsembleEditor.Msg


type Model 
  = Overview State



initState =
  Data.emptyCombo


initModel : Model
initModel = 
  Overview initState


view : (Msg -> msg) -> Model -> msg -> Html msg
view toMsg model done  =
  case model of 
    Overview (scope, ensemble) ->
-- Components.button done [] "Save this combo"
      Components.cols
          [ Components.col [] [
             ScopeEditor.view (\sMsg -> (toMsg <| UpdateScopeEditor sMsg)) (ScopeEditor.Editing scope) done ]
          , Components.col [] [
              EnsembleEditor.view (\msg -> (toMsg <| UpdateEnsembleEditor msg)) (EnsembleEditor.Overview ensemble) done ]
          ]



main = text ""
