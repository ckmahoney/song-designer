module EnsembleEditor exposing (..)


import Browser
import Html exposing (Html, button, div, text, label, p, input)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)


import Types exposing (..)
import View
import Elements
import Tools
import Components
import VoiceEditor


type alias State = List Voice


type Msg
  = CreateVoice State
  | SelectVoice State Int 
  | UpdateVoice State Int Voice -- loops back to Editing  
  | SaveVoice State Int Voice -- loops back to Overview
  | KillVoice State Int -- loops back to Overview
  | Close State -- loops back to Overview

type Model 
  = Overview State
  | Editing State Int Voice


initModel : Model
initModel = 
  Overview []


editor : (Msg -> msg) -> State -> Int -> Voice -> Html msg
editor toMsg state index voice =
  let
    update = (\v -> toMsg <| UpdateVoice state index v)
    close = (\v -> toMsg <| Close state)
    kill = toMsg <| KillVoice state index
  in 
  VoiceEditor.edit voice update close kill


view : (Msg -> msg) -> Model -> msg -> Html msg
view toMsg model done = 
  div [] 
    [ case model of

      Overview state -> 
        if 0 == List.length state then 
          Components.button (toMsg <| CreateVoice state) [] "Add a voice"
        else
          div [Attr.class "is-full has-text-centered"] <|
            Components.button (toMsg <| CreateVoice state) [] "Add Another Voice"
            :: List.indexedMap (\i voice -> 
              div [Attr.class "my-3", Attr.style "border" "1px solid lightgrey",  Attr.style "border-radius" "5px" ] 
               [ Components.colsWith [Attr.class "is-flex"] 
                  [ Components.col [Attr.style "width" "50%"] <| List.singleton <| Components.button (toMsg <| SelectVoice state i) [Attr.class "has-background-primary is-fullwidth has-text-light has-text-weight-bold" ] ("Edit " ++ voice.label) 
                  , Components.col [Attr.style "width" "50%"] <| List.singleton <|Components.deleteButton (toMsg <| KillVoice state i)]
               , VoiceEditor.view voice
               ]) state 


      Editing state index voice ->
        editor toMsg state index voice
    ]

main = text ""
