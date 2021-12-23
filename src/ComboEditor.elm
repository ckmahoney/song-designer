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
  | UpdateLabel String
  | UpdateScope Scope
  | UpdateEnsemble Ensemble Int Voice

type Model 
  = Overview State
  | Scope State ScopeEditor.Model
  | Ensemble State EnsembleEditor.Model


type EditState
  = EScope State ScopeEditor.Msg
  | EEnsemble State EnsembleEditor.Msg

saveScope : State -> ScopeEditor.Msg -> Msg
saveScope ((s,e) as state) msg = 
  case msg of 
    ScopeEditor.Save scope ->
      UpdateScope scope
    _ ->
      Save state
  

router : EditState -> Msg
router incoming =
  case incoming of 
    EScope (scope, e) msg ->
      case msg of
        ScopeEditor.Save next -> 
          UpdateScope next

        ScopeEditor.Update field -> 
          Save (ScopeEditor.change scope field, e)
        
        ScopeEditor.Edit next -> 
          UpdateScope next

        ScopeEditor.Close -> 
          Save (scope, e)

    EEnsemble (s, ensemble) msg ->
      case msg of
        EnsembleEditor.Save next -> Save (s, next)
        _ -> Save (s, ensemble)


update : Msg -> State -> Model
update msg ((s, e) as state) =
  case msg of
    Save next ->
      Overview next

    UpdateLabel label ->
      Overview ({s | label = label}, e)

    UpdateScope scope ->
      Scope (scope, e) <| ScopeEditor.Editing scope

    UpdateEnsemble ensemble int v ->
      Ensemble (s, ensemble) <| EnsembleEditor.Editing ensemble int v


initState =
  Data.emptyCombo


initModel : Model
initModel = 
  Overview initState



thumb : Combo -> Html msg
thumb (scope, ensemble) =
  Components.cols <| 
    [ Components.colHalf <| ScopeEditor.brief scope
    , Components.colHalf <| EnsembleEditor.thumb ensemble
    ]


fromScope : State -> ScopeEditor.Model -> ScopeEditor.State -> State
fromScope (prev, ensemble) mod next =
  (next, ensemble)

fromEnsemble : State ->  EnsembleEditor.Model -> EnsembleEditor.State ->State
fromEnsemble (scope, prev) mod next =
  (scope, next)


view : (State -> msg) -> (Msg -> msg) -> Model -> msg -> Html msg
view changing toMsg model done  =
  case model of 
    Overview ((scope, ensemble) as state)->
      Components.colsMulti
          [ Components.col [Attr.class "columns is-full"]
              [ Components.col [] [ Components.editText "Label" (Components.label scope.label) scope.label (\str -> toMsg <| UpdateLabel str) ]
              , Components.col [] [Components.button (toMsg <| Save state) [Attr.class "is-primary"] "Checkmark" ]   ] 
              , Components.cols <| 
                  [ Components.col [Attr.class "is-half", onClick (toMsg <| UpdateScope scope)] [ ScopeEditor.brief scope ]
                  , Components.colHalf <| EnsembleEditor.picker ensemble (\i -> 
                         let  
                           voice = Tools.getOr i ensemble Data.emptyVoice
                         in
                         toMsg <| UpdateEnsemble ensemble i voice)
                  ]

          ]

    Scope state editor ->
      ScopeEditor.view (changing << fromScope state editor)  editor (\msg -> toMsg <| router (EScope state msg)) (toMsg <| Save state) 

    Ensemble state editor ->
      EnsembleEditor.view (changing << fromEnsemble state editor) editor (\msg -> toMsg <| router (EEnsemble state msg)) (toMsg <| Save state)

main = text ""
