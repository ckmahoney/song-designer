module Components.Player exposing (..)

import Html exposing (Html, div, text, p)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Types exposing (TrackMeta)
import Data
import View 
import Components
import Tools
import Array
import Http
import Url.Builder as Url
import Json.Decode as Decode
import Json.Encode as Encode
import Configs as Conf
import PlaybackPorts exposing (playMusic, pauseMusic, stopMusic, setSource, setAndPlaySource, kill, createSource, createAndPlaySource, getAsset)

type alias NodeId = String
type alias AudioSrc = String


type Model
 = Playing TrackMeta
 | Paused TrackMeta
 | Loading TrackMeta
 | Empty


type Asset
 = MixHigh
 | MixLow
 | Stems
 | Midi


type Msg
  = Yeah


trigger : Msg -> Cmd msg 
trigger msg =
  Cmd.none

apply : Msg -> Model -> Model
apply msg model =
  model


