module Components.Player exposing (..)

import Html exposing (Html, div, text, p)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Defs.Types exposing (TrackMeta)
import Defs.Data as Data
import Components.View as View 
import Components.Components as Components
import Tools
import Array
import Http
import Url.Builder as Url
import Json.Decode as Decode
import Json.Encode as Encode
import Defs.Configs as Conf
import Comm.Ports as Ports
import Comm.PlaybackPorts exposing (playMusic, pauseMusic, stopMusic, setSource, setAndPlaySource, kill, createSource, createAndPlaySource, getAsset)

type alias NodeId = String
type alias AudioSrc = String


type State
 = Playing TrackMeta
 | Paused TrackMeta
 | Loading TrackMeta
 | Empty


type alias Model =
  (NodeId, State)

type Msg
  = Load TrackMeta
  | Loaded
  | Play
  | Pause
  | Finished
  | Stop


jsId : String -> String
jsId id =
  "#" ++ id


new : NodeId -> Model
new nodeId =
  (nodeId, Empty)


default : Model
default =
  ("#the-player", Empty)


noArg : String
noArg = 
  ""


trigger : Msg -> Model -> Cmd msg 
trigger msg ((nodeId, state) as model) =
  case state of 
    Empty -> 
      case msg of 
        Load track ->
          createSource (jsId nodeId, track.filepath)

        _ ->
          Cmd.none 


    Loading track ->
      case msg of
        Loaded ->
          Cmd.batch 
            [ playMusic <| jsId nodeId
            , Ports.scrollTo <| jsId nodeId 
            ]

        _ ->
          Cmd.none

    Paused track ->
      case msg of 
        Play ->
          playMusic <| jsId nodeId

        Stop ->
          stopMusic <| jsId nodeId

        _ ->
          Cmd.none


    Playing track ->
      case msg of 
        Pause ->
          pauseMusic <| jsId nodeId 

        Stop ->
          stopMusic <| jsId nodeId

        Finished ->
          pauseMusic <| jsId nodeId

        _ ->
          Cmd.none


apply : Msg -> Model -> Model
apply msg ((nodeId, state) as model) =
  let to = (\s -> (nodeId, s)) in 
  case state of 
    Empty -> 
      case msg of 
        Load track ->
          to <| Loading track

        _ ->
          model

    Loading track ->
      case msg of
        Loaded ->
          to <| Playing track

        Stop -> 
          to Empty

        _ ->
          model

    Paused track ->
      case msg of 
        Play ->
          to <| Playing track

        Stop ->
          to Empty

        _ ->
          model


    Playing track ->
      case msg of 
        Pause ->
          to <| Paused track

        Stop ->
          to Empty

        Finished ->
           to <| Paused track

        _ ->
          model

    
update : Msg -> Model -> (Model, Cmd msg)
update msg model = 
  (apply msg model, trigger msg model)


isSelected : TrackMeta -> State -> Bool
isSelected track state =
  case state of 
    Empty -> 
      False
    
    Loading t ->
      t == track

    Paused t ->
      t == track

    Playing t ->
      t == track

-- This is the element to be taken over by WaveSurfer.js
node : Model -> Html msg
node (nodeId, _) =
  div [ Attr.id nodeId ] [] 
