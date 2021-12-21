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
  | EditingScope State ScopeEditor.Model 
  | EditingEnsemble State EnsembleEditor.Model


initState =
  Data.emptyCombo


initModel : Model
initModel = 
  Overview initState


view : (Msg -> msg) -> Model -> msg -> Html msg
view toMsg model done  =
  case model of 
    Overview (scope, ensemble) ->
      div [] <| List.singleton <| Components.cols
          [ Components.button done [] "Save this combo"
          , Components.colHalf <| div [onClick <| toMsg <| UpdateScopeEditor <| ScopeEditor.Edit scope]
             [ScopeEditor.view (\sMsg -> (toMsg <| UpdateScopeEditor sMsg)) (ScopeEditor.Overview scope) done]
          , Components.colHalf <| EnsembleEditor.view (\msg -> (toMsg <| UpdateEnsembleEditor msg)) (EnsembleEditor.Overview ensemble) done
          ]


    EditingScope (scope_, ensemble) scope ->
      div []
        [ ScopeEditor.view (\sMsg -> (toMsg <| UpdateScopeEditor sMsg)) scope done
        ]

    EditingEnsemble (scope, ensemble_) eModel ->
     case eModel of 
      EnsembleEditor.Editing ensemble voiceIndex voice ->
       div []
        [ EnsembleEditor.editor (\emsg -> (toMsg <| UpdateEnsembleEditor emsg)) ensemble_ voiceIndex voice
        ]

      _ ->
        text "unhandled ensemble editor case"



main = text ""
