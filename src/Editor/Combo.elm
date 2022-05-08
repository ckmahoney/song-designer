module ComboEditor exposing (..)

import Browser
import Html exposing (Html, button, div, text, label, p, input)
import Html.Attributes as Attr

import Defs.Types exposing (..)
import Components.View as View
import Elements
import Tools
import Components
import Defs.Data
import Editor.Scope
import Editor.Ensemble
import Editor.Voice as VoiceEditor


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
      Overview next

    SaveScope next ->
     Overview  (next,e)

    UpdateScope next ->
      Scope state <| ScopeEditor.Editing next

    PickScope next ->
      Scope state <| ScopeEditor.Editing next

    PickEnsemble next ->
      Ensemble state <| EnsembleEditor.Overview next Nothing

    SaveEnsemble ensemble int v ->
      Ensemble (s, ensemble) <| EnsembleEditor.Editing ensemble int v


initState =
  Data.emptyCombo


border pos =
  [ Attr.style ("border-" ++ pos) "1px solid rgba(0,0,0,0.5)"
  , Attr.style "border-radius" "5px"
  ]


thumb : Combo -> Html msg
thumb (scope, ensemble) =
  Components.colsWith (View.backgroundGradient ["#00e5ff", "#ee00ff"] :: (Attr.class "mx-auto column is-two-thirds is-flex-direction-column my-3" :: (List.append (border "right")  (border "left"))))  <| 
    [ Components.col1 <| ScopeEditor.thumb scope
    , Components.col1 <| EnsembleEditor.thumb ensemble
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


thumbEdit : Model -> (Model -> msg) -> (Combo -> msg) -> (Combo -> msg) ->  Html msg
thumbEdit model forward up close =
  case model of 
    Overview ((scope, ensemble) as state) -> 
     let
        pickScope = forward <| update (PickScope scope) state
        pickEnsemble = forward <| update (PickEnsemble ensemble) state
     in
      Components.colsMulti <| 
        [ Components.colFull <|
              Components.sectionHeading "combo" (Data.helpLink "combo") "Combo Designer" <| List.singleton <| Components.saveButton (close state) "Save Combo"
        , Components.colHalf <|
              ScopeEditor.brief scope pickScope
        , Components.colHalf <|
              EnsembleEditor.brief ensemble pickEnsemble
        ]
   
    Scope ((scope, ensemble) as state) mod ->
      let
        continue = (\m -> forward <| Scope state m)
        keep =  (\s -> up (s, ensemble))
        done = (\s -> forward <| Overview (s, ensemble))
     in
      div [] 
        [ ScopeEditor.view  mod continue keep done
        ] 

    Ensemble ((scope, ensemble) as state) mod ->
     let
        continue = (\m -> forward <| Ensemble state m)
        keep =  (\e -> forward <| update (Save (scope, e)) state)
        done = (\_ -> forward <| Overview state)
     in
      div [] 
        [ EnsembleEditor.view mod continue keep (\ens -> forward <| Overview (scope, ens))
        ] 


main = text ""
