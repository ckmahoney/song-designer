module LayoutEditor exposing (..)

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
import ComboEditor
import ScopeEditor
import EnsembleEditor
import VoiceEditor


type alias State = List Combo


type EditState
  = Open Combo
  | Scope ScopeEditor.Model
  | Ensemble EnsembleEditor.Model


type Msg
  = Close (List Combo)
  | Select (List Combo) Int
  | Update (List Combo) Int Combo
  | Delete (List Combo) Int
  | EditCombo Int Combo EditState


type Model 
  = Overview (List Combo)
  | Editing (List Combo) Int EditState


initState = 
  [ Data.combo1, Data.combo2 ]
  -- []


initModel =
  Just <| Overview initState


init =
  Overview


picker things icon select kill = 
  Components.box
   [ Html.h2 [] [text "Choose a scope"]
   , div [ Attr.class "columns is-multiline level is-vcentered" ] <|
     (List.indexedMap (\i thing ->
       div [ Attr.class "column is-flex is-flex-direction-column is-align-items-center is-justify-content-center" ]
         [ Components.col [ Attr.class "is-full has-text-centered", onClick (select i) ] [(icon thing)]
         , Components.col [ Attr.class "is-full has-text-centered" ] [(Components.deleteIcon (kill i))] 
         ] ) things)
   ]


comboIcon : Combo -> Html msg
comboIcon ((scope, ensemble) as model) =
  div [ Attr.class "box" ] 
    [ label [ Attr.class "label" ] [ text <| scope.label ]
    , Components.svg "ensemble"
    , p [ Attr.class "content" ] [ text <| (String.fromInt <| List.length ensemble) ++ " voices" ]
    ]


edit : EditState -> Int -> Combo -> (EditState -> msg) -> msg -> Html msg
edit state index ((scope_, ensemble_) as combo) toMsg done =
 case Debug.log "has the edit state:" state of 
  Open curr ->   
   let
    fromMsg = (\msg ->
      case msg of 
        ComboEditor.Save next ->
          toMsg (Open next)

        ComboEditor.UpdateScopeEditor sMsg ->
          case sMsg of 
            ScopeEditor.Close scope -> 
              toMsg (Open (scope, ensemble_))

            _ -> -- do nothing yet
              toMsg (Scope <| ScopeEditor.Editing scope_)

        -- Ensembles

        ComboEditor.UpdateEnsembleEditor eMsg ->
          case eMsg of 
            EnsembleEditor.Close next -> 
              toMsg (Open (scope_, Debug.log "saved ensemble in LayoutEditor.edit:" next))

            EnsembleEditor.SelectVoice ensemble voiceIndex ->
             let
               v = Debug.log "Found voice:" <| Tools.getOr (Debug.log "with index:" voiceIndex) ensemble VoiceEditor.newVoice
             in
              toMsg (Ensemble <| EnsembleEditor.Editing ensemble voiceIndex v)

            EnsembleEditor.UpdateVoice  ensemble voiceIndex voice ->
             let
               next = Tools.replaceAt (Debug.log "voice index;" voiceIndex) voice  ensemble
             in
              toMsg (Ensemble <| EnsembleEditor.Editing next voiceIndex voice)

            EnsembleEditor.SaveVoice  ensemble  voiceIndex voice ->
             let
               next = Tools.replaceAt voiceIndex voice  ensemble
             in
              toMsg (Ensemble <| EnsembleEditor.Editing next voiceIndex voice)

            EnsembleEditor.KillVoice ensemble voiceIndex ->
             let
               next = Tools.removeAt voiceIndex ensemble
             in
              toMsg (Open (scope_,  next))

            EnsembleEditor.CreateVoice ensemble ->
             let
               next = VoiceEditor.newVoice ::  ensemble
             in
              toMsg (Open (scope_, next)))
   in 
   ComboEditor.view fromMsg (ComboEditor.Overview curr) done

  
  Scope editor ->
   let 
    updateScope = (\scopeMsg ->
      case scopeMsg of 
        ScopeEditor.Close scope ->
          (toMsg <| Open (scope, ensemble_))

        ScopeEditor.UpdateTitle scope title ->
          (toMsg <| Scope <| ScopeEditor.Editing { scope | label = title })
      
        ScopeEditor.UpdateCPS scope cps ->
          (toMsg <| Scope <| ScopeEditor.Editing { scope | cps = cps })
      
        ScopeEditor.UpdateRoot scope root ->
          (toMsg <| Scope <| ScopeEditor.Editing { scope | root = root })
      
        ScopeEditor.Edit scope ->
          (toMsg <| Scope <| ScopeEditor.Editing scope)
       )
   in
    ScopeEditor.view updateScope editor done

  Ensemble editor ->
    let
     updateEnsemble = (\eMsg ->
      case eMsg of
        EnsembleEditor.Close next ->
          (Open (scope_, next))

        EnsembleEditor.CreateVoice ensemble-> 
         let
           next = VoiceEditor.newVoice :: ensemble
         in
          (Ensemble <| EnsembleEditor.Overview next)

        EnsembleEditor.SelectVoice ensemble voiceIndex -> 
         let
           v = Tools.getOr voiceIndex ensemble VoiceEditor.newVoice
         in
          (Ensemble <| EnsembleEditor.Editing ensemble voiceIndex v)

        EnsembleEditor.UpdateVoice ensemble voiceIndex updated -> 
         let 
           next = Tools.replaceAt voiceIndex updated ensemble_
         in
          (Ensemble <| EnsembleEditor.Editing next voiceIndex updated)

        EnsembleEditor.SaveVoice ensemble voiceIndex updated -> 
         let 
           next = Tools.replaceAt voiceIndex updated ensemble_
         in
          (Ensemble <| EnsembleEditor.Editing next voiceIndex updated)

        EnsembleEditor.KillVoice ensemble voiceIndex  -> 
         let 
           next = Tools.removeAt voiceIndex ensemble
         in
          (Ensemble <| EnsembleEditor.Overview next))

    in
    EnsembleEditor.view (\msg -> toMsg <| updateEnsemble msg)  editor done
     


view : State -> (Int -> msg) -> (Int -> msg) -> Html msg
view layout select kill =
  picker layout View.viewCombo select kill


main = text ""
