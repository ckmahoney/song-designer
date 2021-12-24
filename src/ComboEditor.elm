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
  | PickScope Scope
  | SaveScope Scope
  | SaveEnsemble Ensemble Int Voice

type Model 
  = Overview State
  | Scope State ScopeEditor.Model
  | Ensemble State EnsembleEditor.Model


type EditState
  = EScope State ScopeEditor.Msg
  | EEnsemble State EnsembleEditor.Msg


withScope : State -> Scope -> State
withScope (_, ensemble) scope =
  (scope, ensemble)


withVoice : State -> Ensemble -> Int -> Voice -> State
withVoice (scope, prev) ensemble i voice =
  (scope, Tools.replaceAt i voice ensemble)


edit : EditState -> Msg
edit incoming =
  case incoming of 
    EScope (scope, e) msg ->
      case msg of
        ScopeEditor.Save next -> 
          SaveScope next

        ScopeEditor.Update field -> 
          Save (ScopeEditor.change scope field, e)
              
        ScopeEditor.Edit next -> 
          SaveScope next

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

    SaveScope next ->
     Overview (next,e)

    PickScope next ->
      Scope state <| ScopeEditor.Editing next

    SaveEnsemble ensemble int v ->
      Ensemble (s, ensemble) <| EnsembleEditor.Editing ensemble int v


initState =
  Data.emptyCombo



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



initModel : List State -> Int -> Model
initModel combos index = 
  Overview <| Tools.getOr index combos Data.emptyCombo


thumbEdit : Model -> (Model -> msg) -> Combo -> (Combo -> msg) -> Html msg
thumbEdit model forward ((scope, ensemble) as s) up =
  case Debug.log "model combo:"  model of 
    Overview state -> 
     let
        pickScope = forward <| update (PickScope (Debug.log "picked scope:"  scope)) state
     in
      Components.cols <| 
        [ text "thumbEdit.Overview"
        , Components.col [Attr.class "is-half", onClick pickScope ] <| List.singleton <| ScopeEditor.brief scope
        , Components.colHalf <| EnsembleEditor.thumb ensemble
        ]
   
    Scope state mod ->
     let
        continue = (\m -> forward <| Scope (Debug.log "Found thumbEdit.Scope state:" state) m)
        keep = (\combo -> forward <| update (Save (fromScope state mod combo)) state)
        done = (\_ -> forward <| Overview state)
     in
      div [] 
        [ text "scoping the edit scope: " 
        , ScopeEditor.viewNew mod continue keep done
        ] 

    Ensemble _ _ ->
      text "todo ensembel yay :) "



view : Model -> (Model -> msg) -> (State -> msg) -> msg -> Html msg
view model forward save close  =
  case model of 
    Overview ((scope, ensemble) as state)->
      Components.colsMulti
          [ text "ComboEditor . view " 
          , Components.col [Attr.class "columns is-full"]
              [ Components.col [] [ Components.button close [Attr.class "is-primary"] "Checkmark" ]   ] 
              , Components.cols <| 
                  [ Components.col [Attr.class "is-half", onClick (forward <| (update <| SaveScope scope)  state)] [ ScopeEditor.brief scope ]
                  -- , Components.colHalf <| EnsembleEditor.picker ensemble (\i -> 
                         -- let  
                           -- voice = Tools.getOr i ensemble Data.emptyVoice
                         -- in
                         -- edit <| EEnsemble ensemble i voice)
                  ]

          ]
 
    _ -> 
     text "wip"

    -- Scope state editor ->
    --   ScopeEditor.view (forward << fromScope state editor)  editor (\msg -> toMsg <| edit (EScope state msg)) (toMsg <| Save state) 

    -- -- Ensemble state editor ->
    -- EnsembleEditor.view (forward << fromEnsemble state editor) editor (\msg -> toMsg <| edit (EEnsemble state msg)) (toMsg <| Save state)

main = text ""
