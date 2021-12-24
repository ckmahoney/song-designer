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
  | Local ComboEditor.Model Internal


type Internal 
  = Create
  | Select Int
  | Kill Int
  | Edit Int Combo


type Model 
  = Overview State
  | Editing State Int ComboEditor.Model


apply : State -> Internal -> State
apply state msg =
  case msg of 
   Select index ->
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


curr state index =
  Tools.getOr index state Data.emptyCombo


edit : State -> Internal -> ComboEditor.Model -> Model
edit state msg mod =
  case msg of 
   Select index ->
     Editing state index <| ComboEditor.Overview (curr state index)

   Create -> 
    let
      next = apply state msg
    in
    Overview  next

   Kill index ->
    Overview <| Tools.removeAt index state

   Edit index combo ->
     Editing state index  mod


update : Msg -> State -> Model
update msg state =
  case msg of 
   Save next->
    Overview next

   Update index combo ->
     let
      next = Tools.replaceAt index combo state
     in
      Overview next

   Local mod internal ->
     edit state internal mod

  

initState = 
  [ Data.combo1, Data.combo2 ]
  -- []


initTest = 
  Overview initState
  -- Editing initState 0 <| ComboEditor.initModel initState 0 

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
   , div [ Attr.class "columns is-multiline level is-vcentered  is-flex-direction-column" ] <|
     (List.indexedMap (\i thing ->
       div [ Attr.class "is-clickable column is-flex is-flex-direction-column is-half" ]
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


view : Model -> (Model -> msg) -> (State -> msg) -> (State -> msg) -> Html msg
view model forward save close  =
  case model of 
    Overview state ->
     let 
      kill = (\i -> forward <| update (Save <| Tools.removeAt i state) state)
      select = (\i -> (forward <| edit state (Select i) <| ComboEditor.initModel state i))
     in 
      Components.box <|
        [ Components.button (close state) [] "Close" 
        , picker state View.viewCombo select kill
        , if 4 > List.length state then 
            Components.plusButton (forward <| update (Save (apply state Create)) state)
            else text ""
        ]

    Editing state index mod ->
      let 
        continue = forward << edit state (Edit index (curr state index))
        swap = (\c ->  Tools.replaceAt index c state)
        keep =  (\combo -> forward <| (update (Save <| swap combo) state))
      in 
      Components.box <|
        [ Components.button (close state)  [] "Done!!"
        , ComboEditor.thumbEdit mod continue keep
        ] 


main = text ""
