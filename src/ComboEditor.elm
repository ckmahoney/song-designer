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


update : Msg -> Model -> (Model, Cmd msg)
update msg model = 
  let
    (scope_, ensemble_) = case model of 
      Overview state -> state
      EditingScope state _ -> state
      EditingEnsemble state _ -> state
  in
  case msg of 
    Save combo ->
      (Overview combo, Cmd.none)


    UpdateScopeEditor sMsg ->
      case sMsg of
        ScopeEditor.Over scope ->
          ( Overview (scope, ensemble_), Cmd.none)

        ScopeEditor.Edit scope ->
          ( EditingScope (scope, ensemble_) (ScopeEditor.Editing scope), Cmd.none)

        ScopeEditor.UpdateTitle scope title ->
         let
          next = { scope | label = title }
         in 
           ( EditingScope (next, ensemble_) ( ScopeEditor.Editing next), Cmd.none)

        ScopeEditor.UpdateCPS scope cps ->
         let
          next = { scope | cps = cps }
         in
           ( EditingScope (next, ensemble_) ( ScopeEditor.Editing next) , Cmd.none)

        ScopeEditor.UpdateRoot scope root ->
         let
          next = { scope | root = root }
         in
           ( EditingScope (next, ensemble_) ( ScopeEditor.Editing next), Cmd.none)

    UpdateEnsembleEditor eMsg ->
      case eMsg of 
        EnsembleEditor.CreateVoice -> 
         let
           next = VoiceEditor.newVoice :: ensemble_
         in
          (EditingEnsemble (scope_, next) (EnsembleEditor.Overview next), Cmd.none)

        EnsembleEditor.SelectVoice state index -> 
         let
           v = Tools.getOr index state VoiceEditor.newVoice
         in
          (EditingEnsemble (scope_, ensemble_) (EnsembleEditor.Editing ensemble_ index v), Cmd.none)

        EnsembleEditor.UpdateVoice state index updated -> 
         let 
           next = Tools.replaceAt index updated state
         in
          (EditingEnsemble (scope_, next) (EnsembleEditor.Editing next index updated), Cmd.none)

        EnsembleEditor.SaveVoice state index updated -> 
         let 
           next = Tools.replaceAt index updated state
         in
          (EditingEnsemble (scope_, next) (EnsembleEditor.Editing next index updated), Cmd.none)

        EnsembleEditor.KillVoice state index  -> 
         let 
           next = Tools.removeAt index state
         in
          (EditingEnsemble (scope_, next) (EnsembleEditor.Overview next), Cmd.none)


view : (Msg -> msg) -> Model -> Html msg
view toMsg model =
  case model of 
    Overview (scope, ensemble) ->
      div [] 
        [ div [onClick <| toMsg <| UpdateScopeEditor <| ScopeEditor.Edit scope]
           [ ScopeEditor.view (\sMsg -> (toMsg <| UpdateScopeEditor sMsg)) (ScopeEditor.Overview scope) ] 
        ]

    EditingScope (scope_, ensemble) scope ->
      div []
        [ ScopeEditor.view (\sMsg -> (toMsg <| UpdateScopeEditor sMsg))  scope
        ]

    EditingEnsemble (scope, ensemble_) ensemble ->
      div []
        [ EnsembleEditor.view (\eMsg -> (toMsg <| UpdateEnsembleEditor eMsg))  ensemble
        ]


main = text ""
