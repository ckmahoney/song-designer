module ScoreMeta exposing (..)

import Browser
import Html exposing (Html, h1, button, div, text, label, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Types exposing (..)
import Data
import View 
import Components
import Tools
import Http
import Url.Builder as Url
import Json.Decode as Decode
import Json.Encode as Encode
import Encoders as JE
import Configs as Conf


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


readonly : Model -> msg -> Html msg
readonly meta revise = 
  Components.box
    [ Components.label meta.title
    , Components.button revise [] "Edit"
    -- , Components.button done [] "Return"
    ]


editor : Model -> Html msg
editor meta =
  Components.box
    [ text <|  "Editing Meta " ++ meta.title
    -- , Components.editText "Title" (text "") card.title (\str -> (change <| SetTitle str))
    -- , Components.keyPickerFull useSharps card.key (\int -> change <| SetKey int)
    -- , Components.button save [] "Save changes" 
    -- , Components.button cancel [] "Cancel" 
    ]


main = text ""
