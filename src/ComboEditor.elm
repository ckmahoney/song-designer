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


view : (Msg -> msg) -> Model -> Html msg
view toMsg model =
  case model of 
    Overview (scope, ensemble) ->
      div [] <| List.singleton <| Components.cols
          [ Components.colHalf <| div [onClick <| toMsg <| UpdateScopeEditor <| ScopeEditor.Edit scope]
             [ScopeEditor.view (\sMsg -> (toMsg <| UpdateScopeEditor sMsg)) (ScopeEditor.Overview scope)]
          , Components.colHalf <| EnsembleEditor.view (\msg -> (toMsg <| UpdateEnsembleEditor msg)) (EnsembleEditor.Overview ensemble)
             -- List.indexedMap (\i voice ->
              -- div [onClick <| toMsg <| UpdateEnsembleEditor <| EnsembleEditor.SelectVoice ensemble i] [VoiceEditor.icon voice]) ensemble
          ]


    EditingScope (scope_, ensemble) scope ->
      div []
        [ ScopeEditor.view (\sMsg -> (toMsg <| UpdateScopeEditor sMsg)) scope
        ]

    EditingEnsemble _ eModel ->
     case eModel of 
      EnsembleEditor.Editing ensemble voiceIndex voice ->
       div []
        [ EnsembleEditor.editor (\emsg -> (toMsg <| UpdateEnsembleEditor emsg)) ensemble voiceIndex voice
        ]

      _ ->
        text "unhandled ensemble editor case"


-- update : Msg -> Model -> (Model, Cmd msg)
-- update msg model = 
--   let
--     (scope_, ensemble_) = case model of 
--       Overview state -> state
--       EditingScope state _ -> state
--       EditingEnsemble state _ -> state
--   in
--   case msg of 
--     Save combo ->
--       (Overview combo, Cmd.none)


--     UpdateScopeEditor sMsg ->
--       case sMsg of
--         ScopeEditor.Close scope ->
--           ( Overview (scope, ensemble_), Cmd.none)

--         ScopeEditor.Edit scope ->
--           ( EditingScope (scope, ensemble_) (ScopeEditor.Editing scope), Cmd.none)

--         ScopeEditor.UpdateTitle scope title ->
--          let
--           next = { scope | label = title }
--          in 
--            ( EditingScope (next, ensemble_) ( ScopeEditor.Editing next), Cmd.none)

--         ScopeEditor.UpdateCPS scope cps ->
--          let
--           next = { scope | cps = cps }
--          in
--            ( EditingScope (next, ensemble_) ( ScopeEditor.Editing next) , Cmd.none)

--         ScopeEditor.UpdateRoot scope root ->
--          let
--           next = { scope | root = root }
--          in
--            ( EditingScope (next, ensemble_) ( ScopeEditor.Editing next), Cmd.none)


main = text ""
