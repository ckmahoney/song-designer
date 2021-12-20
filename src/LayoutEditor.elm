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


type alias State = List Combo


type Msg
  = Close (List Combo)
  | Select (List Combo) Int
  | Update (List Combo) Int Combo
  | Delete (List Combo) Int
  | EditCombo Int ComboEditor.Msg


type Model 
  = Overview (List Combo)
  | Editing (List Combo) Int Combo




initState = 
  [Data.combo1, Data.combo2]
-- []


initModel =
  Overview initState


init =
  Overview


picker things icon select  = 
  Components.box
   [ Html.h2 [] [text "Choose a scope"]
   , div [ Attr.class "columns is-multiline level is-vcentered" ] <|
     (List.indexedMap (\i thing ->
       div [ Attr.class "column is-flex is-flex-direction-column is-align-items-center is-justify-content-center" ]
         [ Components.col [ Attr.class "is-full has-text-centered", onClick (select i) ] [(icon thing)]
         -- , Components.col [ Attr.class "is-full has-text-centered" ] [(Components.deleteIcon (kill i))] 
         ] ) things)
  ]

comboIcon : Combo -> Html msg
comboIcon ((scope, ensemble) as model) =
  div [ Attr.class "box" ] 
    [ label [ Attr.class "label" ] [ text <| scope.label ]
    , Components.svg "ensemble"
    , p [ Attr.class "content" ] [ text <| (String.fromInt <| List.length ensemble) ++ " voices" ]
    ] 

edit : Combo -> (Combo -> msg) -> msg -> Html msg
edit combo update done =
  div [onClick done]
    [ comboIcon combo ] 

view :  State -> (Int -> msg) ->Html msg
view layout select =
  picker layout View.viewCombo select


-- view : State -> Html msg
-- view layout =
--     Components.box 
--     <| List.map comboIcon layout


main = text ""
