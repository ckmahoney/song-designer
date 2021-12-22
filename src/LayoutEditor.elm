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


look things icon =
  Components.box
   [ Html.h2 [ Attr.class "subtitle" ] [text "Layout"]
   , div [ Attr.class "columns is-multiline level is-vcentered" ]
       [ p [ Attr.class "content"] 
           [ text "Here is your layout."
           , Html.br [] []
           , Html.br [] []
           , text "Each combo has a name, some voices, and duration in seconds."] ]
   , div [ Attr.class "columns is-multiline level is-vcentered is-flex is-justify-content-flex-start" ] <|
     (List.indexedMap (\i thing ->
       div [ Attr.class "is-flex is-flex-direction-column" ]
         [ Components.col [ Attr.class "is-full has-text-centered" ] [(icon thing)]
         ] ) things)

   ]


picker things icon select kill another = 
  Components.box
   [ Html.h2 [ Attr.class "subtitle" ] [text "Layout"]
   , div [ Attr.class "columns is-multiline level is-vcentered" ]
       [ p [ Attr.class "content"] 
           [ text "Organize the parts of your sound. "    
           , Html.br [] []
           , Html.br [] []
           , text "Click on a scope to change the details and voices." ] 
           , Html.br [] []]
   , div [ Attr.class "columns is-multiline level is-vcentered" ] <|
     (List.indexedMap (\i thing ->
       div [ Attr.class "is-clickable column is-flex is-flex-direction-column" ]
         [ Components.col [ Attr.class "is-full has-text-centered", onClick (select i) ] [(icon thing)]
         , Components.col [ Attr.class "is-full has-text-centered" ] [(Components.deleteIcon (kill i))] 
         ] ) things)
   , if 4 > List.length things then 
     Components.button another [Attr.class "is-primary"] "Add Another Combo"
     else text ""
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
 case state of 
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


            ScopeEditor.UpdateTitle scope title ->
              (toMsg <| Scope <| ScopeEditor.Editing { scope | label = title })

            ScopeEditor.UpdateCPS scope cps ->
              (toMsg <| Scope <| ScopeEditor.Editing { scope | cps = cps })

            ScopeEditor.UpdateCPC scope cpc ->
              (toMsg <| Scope <| ScopeEditor.Editing { scope | cpc = cpc })

            ScopeEditor.UpdateRoot scope root ->
              (toMsg <| Scope <| ScopeEditor.Editing { scope | root = root })

            ScopeEditor.Edit scope ->
              (toMsg <| Scope <| ScopeEditor.Editing scope)
        -- Ensembles

        ComboEditor.UpdateEnsembleEditor eMsg ->
          case eMsg of 
            EnsembleEditor.Close next -> 
              toMsg (Open (scope_,  next))

            EnsembleEditor.SelectVoice ensemble voiceIndex ->
             let
               v = Tools.getOr voiceIndex ensemble VoiceEditor.newVoice
             in
              toMsg (Ensemble <| EnsembleEditor.Editing ensemble voiceIndex v)

            EnsembleEditor.UpdateVoice  ensemble voiceIndex voice ->
             let
               next = Tools.replaceAt voiceIndex voice  ensemble
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

    updateScope = (\scopeMsg ->
      case scopeMsg of 
        ScopeEditor.Close scope ->
          (toMsg <| Open (scope, ensemble_))

        ScopeEditor.UpdateTitle scope title ->
          (toMsg <| Scope <| ScopeEditor.Editing { scope | label = title })
      
        ScopeEditor.UpdateCPS scope cps ->
          (toMsg <| Scope <| ScopeEditor.Editing { scope | cps = cps })

        ScopeEditor.UpdateCPC scope cpc ->
          (toMsg <| Scope <| ScopeEditor.Editing { scope | cpc = cpc })
      
        ScopeEditor.UpdateRoot scope root ->
          (toMsg <| Scope <| ScopeEditor.Editing { scope | root = root })
      
        ScopeEditor.Edit scope ->
          (toMsg <| Scope <| ScopeEditor.Editing scope)
       )
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

    saveQuit = (\msg -> (fromMsg <| ComboEditor.UpdateEnsembleEditor msg))

   in 
   div []
    [ Components.col [] <| [ Components.button done [] "Save Combo"]
    , Components.colsWith [Attr.class "is-hidden-mobile"]
          [ Components.colHalf <|
              ScopeEditor.view1 updateScope scope_ 
          , Components.colHalf <|
              EnsembleEditor.view saveQuit (EnsembleEditor.Overview ensemble_) done 
          ]
    , Components.colsWith [Attr.class "is-hidden-tablet"]
          [ Components.col [] [
              ScopeEditor.view1 updateScope scope_ ]
          , Components.col [Attr.class "is-full has-text-centerd"] [
              EnsembleEditor.view saveQuit (EnsembleEditor.Overview ensemble_) done ]
          ]
    ]

   -- ComboEditor.view fromMsg (ComboEditor.Overview curr) done

  
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
      

        ScopeEditor.UpdateCPC scope cpc ->
          (toMsg <| Scope <| ScopeEditor.Editing { scope | cpc = cpc })

        ScopeEditor.UpdateRoot scope root ->
          (toMsg <| Scope <| ScopeEditor.Editing { scope | root = root })
      
        ScopeEditor.Edit scope ->
          (toMsg <| Scope <| ScopeEditor.Editing scope)
       )
   in
   case editor of 
     ScopeEditor.Overview s ->
      div []
       [ Components.col [] <| [ Components.button done [] "Save Combo"]
       , Components.colsWith [Attr.class "is-hidden-mobile"]
          [ ScopeEditor.view1 updateScope s]
       , Components.colsWith [Attr.class "is-hidden-tablet"]
          [ ScopeEditor.viewMobile updateScope(ScopeEditor.Editing s) done  ]
       ]

     ScopeEditor.Editing s ->
      div []
       [ Components.col [] <| [ Components.button done [] "Save Combo"]
       , Components.colsWith [Attr.class "is-hidden-mobile"]
          [ ScopeEditor.view1 updateScope s ]
       , Components.colsWith [Attr.class "is-hidden-tablet"]
          [ ScopeEditor.viewMobile updateScope(ScopeEditor.Editing s) done  ]
       ]


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
   div []
    [ EnsembleEditor.view (\msg -> toMsg <| updateEnsemble msg)  editor done
    ]

     


view : State -> (Int -> msg) -> (Int -> msg) -> msg ->Html msg
view layout select kill addAnother =
  picker layout View.viewCombo select kill addAnother


main = text ""
