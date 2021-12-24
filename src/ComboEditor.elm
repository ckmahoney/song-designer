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
  | PickEnsemble Ensemble
  | UpdateScope Scope
  | SaveScope Scope
  | SaveEnsemble Ensemble Int Voice

type Model 
  = Overview State
  | Scope State ScopeEditor.Model
  | Ensemble State EnsembleEditor.Model




withScope : State -> Scope -> State
withScope (_, ensemble) scope =
  (scope, ensemble)


withVoice : State -> Ensemble -> Int -> Voice -> State
withVoice (scope, prev) ensemble i voice =
  (scope, Tools.replaceAt i voice ensemble)


update : Msg -> State -> Model
update msg ((s, e) as state) =
  case msg of
    Save next ->
      Overview (Debug.log "Saving this new state::" next)

    SaveScope next ->
     Overview (Debug.log "completed save for new scope in combo:"  (next,e))

    UpdateScope next ->
      Scope state <| ScopeEditor.Editing next

    PickScope next ->
      Scope state <| ScopeEditor.Editing next

    PickEnsemble next ->
      Ensemble state <| EnsembleEditor.Overview next

    SaveEnsemble ensemble int v ->
      Ensemble (s, ensemble) <| EnsembleEditor.Editing ensemble int v


initState =
  Data.emptyCombo



thumb : Combo -> Html msg
thumb (scope, ensemble) =
  Components.cols <| 
    [ Components.colHalf <| ScopeEditor.brief scope
    , Components.colHalf <| View.ensembleThumb ensemble
    ]


fromScope : State -> ScopeEditor.Model -> ScopeEditor.State -> State
fromScope (prev, ensemble) mod next =
  (Debug.log "fromScope.next:" next, ensemble)

fromEnsemble : State ->  EnsembleEditor.Model -> EnsembleEditor.State ->State
fromEnsemble (scope, prev) mod next =
  (scope, next)



initModel : List State -> Int -> Model
initModel combos index = 
  Overview <| Tools.getOr index combos Data.emptyCombo


thumbEdit : Model -> (Model -> msg) -> (Combo -> msg) -> Html msg
thumbEdit model forward up =
  case Debug.log "model combo:"  model of 
    Overview ((scope, ensemble) as state) -> 
     let
        pickScope = forward <| update (PickScope scope) state
        pickEnsemble = forward <| update (PickEnsemble ensemble) state
     in
      Components.cols <| 
        [ Components.col [Attr.class "is-half", onClick pickScope ] <| List.singleton <| 
              ScopeEditor.brief scope
        , Components.col [Attr.class "is-half", onClick pickEnsemble ] <| List.singleton <| 
              View.ensemblePeek ensemble
        ]
   
    Scope ((scope, ensemble) as state) mod ->
     let
        continue = (\m -> forward <| Scope state m)
        keep =  (\s -> up (s, ensemble))
        done = (\_ -> forward <| Overview state)
     in
      div [] 
        [ text "scoping the edit scope: " 
        , ScopeEditor.view  mod continue keep done
        ] 

    Ensemble ((scope, ensemble) as state) mod ->
     let
        continue = (\m -> forward <| Ensemble state m)
        keep =  (\e -> up (scope, e))
        done = (\_ -> forward <| Overview state)
     in
      div [] 
        [ text "scoping the edit scope: " 
        , EnsembleEditor.view mod continue keep done
        ] 


main = text ""
