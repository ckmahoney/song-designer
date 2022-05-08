module Editor.ScoreMeta exposing (..)

import Browser
import Html exposing (Html, h1, button, div, text, label, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Defs.Types exposing (..)
import Defs.Data
import Components.View as View 
import Components.Components as Components
import Tools
import Http
import Url.Builder as Url
import Json.Decode as Decode
import Json.Encode as Encode
import Comm.Encoders as JE
import Defs.Configs as Conf


type alias Title = String
type alias Key = Int
type alias Tempo = Float -- string representation of the BPM
type alias Bpm = Float
type alias Cpc = Int

useSharps = False


type Style
  = Beat
  | Groove
  | Mix
  | Instrumental
  | Abstract


type alias Model = 
  { title : String
  , tempo : String
  , bpm : Bpm
  , key : Key
  , cpc : Cpc
  }


type Msg 
  = SetTitle String
  | SetBpm String
  | SetKey Int
  | SetCpc Int


type State 
  = Viewing Model 
  | Editing Model Model

maxDuration : Float
maxDuration = 
  5 * 60.0


duration : Float -> Int -> Int -> Float
duration cps cpc nCycles =
  (toFloat cpc) * (1/cps) * (toFloat nCycles)


tempoMessage : Int -> Float -> Int -> Html msg
tempoMessage cpc cps nCycles =
  let 
   dur = duration cps cpc nCycles
  in 
  div [] 
    [ p [] [ text <| View.timeString <| dur ]
    , if dur < maxDuration then text "" else 
        p [] [ text "Synthony currently supports songs up to 5 minutes in length. You can make a layout that looks longer than that, but the song writer will shorten it to fit into the 5 minute range." ]
    ]
  


bpmMin = 44.0
bpmMax = 180.0


empty : Model
empty =
  Model "New Song" "110" 110 6 4 


edit : Msg -> Model -> Model
edit msg meta = 
  case msg of 
    SetTitle title -> 
      { meta | title = title}

    SetKey index -> 
      { meta | key = index }

    SetBpm tempoString -> 
     let 
       c = Maybe.withDefault meta.bpm <| String.toFloat tempoString
       bpm = if c < bpmMin then bpmMin else if c > bpmMax then bpmMax else c
     in 
     { meta | tempo = tempoString, bpm = bpm }

    SetCpc cpc ->
      { meta | cpc = cpc }


apply : Msg -> State -> State
apply msg state =
  case state of 
    Editing orig next -> 
      case msg of 
        _ -> Editing orig <| edit msg next

    Viewing item -> 
      case msg of 
       _ -> Viewing item


update : Msg -> State -> (State, Cmd msg) 
update msg state =
  (apply msg state, Cmd.none)


initState : State
initState = Editing empty empty


readonly : Int -> Model -> msg -> Html msg
readonly nCycles meta revise = 
  div [class "box m-auto", style "width" "360px" ]
    [ Components.label meta.title
    , p [] [ text <| Components.keyMessage useSharps meta.key ]
    , p [] [ text <| (String.fromFloat meta.bpm) ++ " Beats Per Minute" ]
    , tempoMessage meta.cpc  (meta.bpm / 60) nCycles 
    , Components.button revise [class "mt-3"] "Edit Details"
    ]


editor : Model -> (Msg -> msg) -> msg -> msg -> Html msg
editor meta change save cancel =
  Components.box
    [ Components.editText "Title" (text "") meta.title (\str -> (change <| SetTitle str))
    , Components.editRangeString "BPM" (text "Enter a BPM between 44 and 180.") (bpmMin, bpmMax) meta.tempo (\str -> (change <| SetBpm str))
    , Components.keyPickerFull useSharps meta.key (\int -> change <| SetKey int)
    , Components.button save [class "mr-3"] "Save changes" 
    , Components.button cancel [] "Cancel" 
    ]


main = text ""
