module EnsembleEditor exposing (..)


import Browser
import Html exposing (Html, button, div, text, label, p, input)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)


import Types exposing (..)
import Data
import View
import Elements
import Tools
import Components
import VoiceEditor


type alias State = List Voice


type Msg
  = Create
  | Select Int 
  | Update Int Voice
  | Kill Int
  | Save State


type Model 
  = Overview State
  | Editing State Int Voice


update : Msg -> State -> Model
update msg state =
  case msg of 
    Save next ->
      Overview next

    Create -> 
     let
       v = VoiceEditor.newVoice
       next = List.append state [ v ]
     in
      Editing next (List.length next) v

    Select voiceIndex -> 
      Editing state voiceIndex <| Tools.getOr voiceIndex state VoiceEditor.newVoice

    Update voiceIndex updated -> 
     let 
       next = Tools.replaceAt voiceIndex updated state
      in
      Editing next voiceIndex updated

    Kill voiceIndex  -> 
      Overview <| Tools.removeAt voiceIndex state


thumb : State -> Html msg
thumb state =
   Components.box <| List.singleton <| Components.colsWith [Attr.class "is-multiline is-mobile"]
     <| if List.length state == 0 then 
       [ text "No voices in this ensemble." ] else 
       List.map (\{role} -> Components.colSize "is-one-quarter" <| Components.svg  (Tuple.first <| Data.roleLabel role)) state

picker : State -> (Int -> msg) -> Html msg
picker state click =
   Components.box <| List.singleton <| Components.colsWith [Attr.class "is-multiline is-mobile"]
     <| if List.length state == 0 then 
       [ text "No voices in this ensemble." ] else 
       List.indexedMap (\i {role} -> Components.col [onClick (click i), Attr.class "is-one-quarter"] <| List.singleton <| Components.svg  (Tuple.first <| Data.roleLabel role)) state


brief : State -> Html msg
brief state =
  Components.box <| if List.length state == 0 then 
    [ text "No voices in this ensemble." ] else 
    List.map (\{role} -> View.roleIcon role) state


initModel : Model
initModel = 
  Overview []


editor : (Msg -> msg) -> State -> Int -> VoiceEditor.Model -> Html msg
editor toMsg state index model =
  let
    up = (\v -> toMsg <| Update index v)
    close = (\v -> toMsg <| Save state)
    kill = toMsg <| Kill index
  in 
  VoiceEditor.edit model up close kill

voiceGrid : State -> (Msg -> msg) -> Html msg
voiceGrid state toMsg =
  let
    next = List.append state [ Data.emptyVoice ]
    i = List.length next
    createVoice =  toMsg <| Select i
  in 
  div [] <|
    Components.button createVoice [ Attr.class "column is-one-quarter"] "+" 
    :: List.indexedMap (\index {role} -> 
         div [Attr.class "column is-one-quarter", onClick <| toMsg <| Select index] [View.roleIcon role]) state 

editorNew :State -> Int -> Voice -> (Msg -> msg) -> Html msg
editorNew state index voice toMsg =
  let
    up = (\v -> toMsg <| Update index v)
    close = (\v -> toMsg <| Save state)
    kill = toMsg <| Kill index
  in 
  div []
    [ VoiceEditor.edit voice up close kill
    , voiceGrid state toMsg
    ]


showNoVoices : State -> (Msg -> msg) -> Html msg
showNoVoices state toMsg =
  Components.button (toMsg <| Create) [] "Create the First Voice"  


listVoices : State -> (Msg -> msg) -> Html msg
listVoices state toMsg =
  div [Attr.class "is-full has-text-centered"] <|
   List.indexedMap (\i voice -> 
      div [Attr.class "my-3", Attr.style "border" "1px solid lightgrey",  Attr.style "border-radius" "5px" ] 
       [ Components.colsWith [Attr.class "is-flex"] 
          [ Components.col [Attr.style "width" "50%"] <| List.singleton <| Components.button (toMsg <| Select  i) [Attr.class "has-background-primary is-fullwidth has-text-light has-text-weight-bold" ] ("Edit " ++ voice.label) 
          , Components.col [Attr.style "width" "50%"] <| List.singleton <|Components.deleteButton (toMsg <| Kill  i)]
       , VoiceEditor.view voice
       ]) state 


display : Model -> (Msg -> msg) -> Html msg
display model toMsg =
 case model of
  Overview state -> 
    if 0 == List.length state then 
      showNoVoices state toMsg
    else
      div [ Attr.class "is-flex is-flex-direction-column align-items-flex-start" ] <|
        [ (Components.addButton (toMsg <| Create ) "Add a Voice")
        , listVoices state toMsg 
        ]

  Editing state index mod ->
    editor toMsg state index mod


view : (State -> msg) -> Model -> (Msg -> msg) -> msg -> Html msg
view changer model toMsg close = 
  Components.box 
    [ Components.button close [] "Save Ensemble"
    , display model toMsg
    ] 


main = text ""
