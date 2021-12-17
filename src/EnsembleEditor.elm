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
  = CreateVoice
  | SelectVoice State Int 
  | UpdateVoice State Int Voice -- loops back to Editing  
  | SaveVoice State Int Voice -- loops back to Overview
  | KillVoice State Int -- loops back to Overview


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
    save = (\v -> toMsg <| SaveVoice state index v)
    kill = toMsg <| KillVoice state index
  in 
   VoiceEditor.edit voice update save kill


view : (Msg -> msg) -> Model -> Html msg
view toMsg model = 
  case model of 
    Overview state -> 
      if 0 == List.length state then 
        Components.button (toMsg CreateVoice) [] "Add a voice"
      else 
        div [] <|         
         Components.button (toMsg CreateVoice) [] "Add a voice" 
         :: List.indexedMap (\i voice -> 
            div [onClick <| toMsg <| SelectVoice state i] [VoiceEditor.view voice]) state

    Editing state index voice ->
      editor toMsg state index voice


main = text ""
