port module Playback exposing (..)


import Html
import Types as T
import Data
import View 
import Components
import Tools
import Array
import Http
import Url.Builder as Url
import Json.Decode as Decode
import Json.Encode as Encode


port playMusic : String -> Cmd msg

port pauseMusic : String -> Cmd msg

port stopMusic : String -> Cmd msg

port setSource : String -> Cmd msg


type Model
 = Playing T.TrackMeta
 | Paused T.TrackMeta
 | Stopped 
 


type Msg
  = Load T.TrackMeta
  | Play 
  | Pause
  | Stop
  | Select (Maybe T.TrackMeta)


fx : Msg -> Model -> Cmd msg
fx msg model =
  case msg of 
    Load track -> setSource track.filepath
    Select (Nothing) -> setSource ""
    Select (Just track) -> setSource track.filepath
    _ -> Cmd.none
 

update : Msg -> Model -> Model
update msg model =
  case msg of  
    Load track -> 
      Paused track

    Select (Nothing) -> Stopped
    Select (Just track) -> Paused track

    Play ->
      case model of 
        Playing t -> model
        Paused t -> Playing t
        _ -> model


    Pause ->
      case model of 
        Playing t -> Paused t
        _ -> model


    Stop ->
      Stopped


main = Html.text ""
