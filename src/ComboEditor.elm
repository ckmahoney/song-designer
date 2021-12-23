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

saveScope : State -> ScopeEditor.Msg -> Msg
saveScope ((s,e) as state) msg = 
  case msg of 
    ScopeEditor.Save scope ->
      SaveScope scope
    _ ->
      Save state
  

router : EditState -> Msg
router incoming =
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

    SaveScope scope ->
      Scope (scope, e) <| ScopeEditor.Editing scope

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

thumbEdit : Combo -> (Combo -> msg) -> Html msg
thumbEdit combo up =
  text "updating"


fromScope : State -> ScopeEditor.Model -> ScopeEditor.State -> State
fromScope (prev, ensemble) mod next =
  (next, ensemble)

fromEnsemble : State ->  EnsembleEditor.Model -> EnsembleEditor.State ->State
fromEnsemble (scope, prev) mod next =
  (scope, next)



initModel : List State -> Int -> Model
initModel combos index = 
  Overview <| Tools.getOr index combos Data.emptyCombo


view : Model -> (Model -> msg) -> (State -> msg) -> msg -> Html msg
view model forward save close  =
  case model of 
    Overview ((scope, ensemble) as state)->
      Components.colsMulti
          [ Components.col [Attr.class "columns is-full"]
              [ Components.col [] [ Components.button close [Attr.class "is-primary"] "Checkmark" ]   ] 
              , Components.cols <| 
                  [ Components.col [Attr.class "is-half", onClick (forward <| (update <| SaveScope scope)  state)] [ ScopeEditor.brief scope ]
                  -- , Components.colHalf <| EnsembleEditor.picker ensemble (\i -> 
                         -- let  
                           -- voice = Tools.getOr i ensemble Data.emptyVoice
                         -- in
                         -- router <| EEnsemble ensemble i voice)
                  ]

          ]
 
    _ -> 
     text "wip"

    -- Scope state editor ->
    --   ScopeEditor.view (forward << fromScope state editor)  editor (\msg -> toMsg <| router (EScope state msg)) (toMsg <| Save state) 

    -- -- Ensemble state editor ->
    -- EnsembleEditor.view (forward << fromEnsemble state editor) editor (\msg -> toMsg <| router (EEnsemble state msg)) (toMsg <| Save state)

main = text ""
