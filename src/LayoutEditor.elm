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
  = Save State
  | Create
  | Open  Int
  | Select Int
  | Update Int ComboEditor.Msg
  | Kill Int


type Model 
  = Overview State
  | Editing State Int ComboEditor.Model


update : Msg -> State -> Model
update msg state =
  case msg of 
    _ -> 
      Overview state
  

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


view : State -> (Int -> msg) -> (Int -> msg) -> msg ->Html msg
view layout select kill addAnother =
  picker layout View.viewCombo select kill addAnother


look : State -> List (Html msg)
look layout =
  List.map ComboEditor.thumb layout


editor : (Msg -> msg) -> State -> Int -> ComboEditor.Model -> Html msg
editor toMsg state index comboModel =
  ComboEditor.view (\msg -> toMsg <| Update index msg) comboModel (toMsg <| Save state) 


viewNew : Model -> (Msg -> msg) -> Html msg
viewNew model toMsg  =
  case model of 
    Overview state ->
      Components.box <|
        Components.plusButton (toMsg Create)
        :: look state

    Editing state index comboModel ->
      editor toMsg state index comboModel 


main = text ""
