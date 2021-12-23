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


type Msg
  = Update Int Combo
  | Save State
  | Local Internal


type Internal 
  = Create
  | Select Int
  | Kill Int
  | Edit Int Combo


type Model 
  = Overview State
  | Editing State Int


apply : State -> Internal -> State
apply state msg =
  case msg of 
   Select _ ->
      state 

   Create -> 
    let
      v = Data.emptyCombo
      next = List.append state [ v ]
      index = List.length next
    in 
    next 

   Kill index ->
    Tools.removeAt index state


   Edit index combo ->
     Tools.replaceAt index combo state


edit : State -> Internal -> Model
edit state msg =
 let
  next = apply state  msg
 in 
  case msg of 
   Select index ->
     Editing state index

   Create -> 
    let
      v = Data.emptyCombo
      n = List.append state [ v ]
      index = List.length n
    in 
    Editing next index

   Kill index ->
    Overview <| Tools.removeAt index state


   Edit index combo ->
     Editing (Tools.replaceAt index combo state) index


update : Msg -> State -> Model
update msg state =
  case msg of 
   Save next->
    Overview next


   Update index combo ->
     let
      next = Tools.replaceAt index combo state
     in
      Editing next index

   Local x ->
     edit state x

  

initState = 
  [ Data.combo1, Data.combo2 ]
  -- []


initModel =
  Just <| Overview initState


init =
  Overview


lookOld things icon =
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


picker things icon select kill  = 
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

   ]


comboIcon : Combo -> Html msg
comboIcon ((scope, ensemble) as model) =
  div [ Attr.class "box" ] 
    [ label [ Attr.class "label" ] [ text <| scope.label ]
    , Components.svg "ensemble"
    , p [ Attr.class "content" ] [ text <| (String.fromInt <| List.length ensemble) ++ " voices" ]
    ]


look : State -> List (Html msg)
look layout =
  List.map ComboEditor.thumb layout




create : State -> State
create state  =
 List.append state [ Data.emptyCombo ]
 


createMsg : State -> Msg
createMsg state = 
  let 
   next = create state
   i = List.length next
   v = Tools.getOr
  in 
  Save next


fromCombo : State -> Int -> Combo -> State
fromCombo state index combo =
  Tools.replaceAt index combo state


editor : State -> Int -> (State -> msg) -> (State -> msg) -> msg -> Html msg
editor state index up save close  =
  let 
    upper = (\c -> (up (fromCombo state index c)))
    combo = Tools.getOr index state Data.emptyCombo
  in 
  ComboEditor.view combo upper close


viewNew : Model -> (State -> msg) -> (State -> msg) -> msg -> Html msg
viewNew model up save close  =
  case Debug.log "has model:" model of 
    Overview state ->
      Components.box <|
        [ Components.button close [] "Save"
        , Components.plusButton (save (apply state Create))
        , picker state View.viewCombo (\i -> (up <| apply state <| Select i)) (\i -> (up <| apply state <| Kill i))
        , if 4 > List.length state then 
            Components.button (up (apply state Create)) [Attr.class "is-primary"] "Add Another Combo"
            else text ""
        ]

    Editing state index ->
      editor state index up save close


main = text ""
