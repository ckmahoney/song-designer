port module PlaybackPorts exposing (..)

import Html exposing (text)


type alias NodeId = String
type alias AudioSrc = String
type alias NoArgs = String


port playMusic : NoArgs -> Cmd msg

port pauseMusic : NoArgs -> Cmd msg

port stopMusic : NoArgs -> Cmd msg

port setSource : (NodeId, AudioSrc) -> Cmd msg

port setAndPlaySource : (NodeId, AudioSrc) -> Cmd msg

port kill : NoArgs -> Cmd msg

port createSource : (NodeId, AudioSrc) -> Cmd msg

port createAndPlaySource : (NodeId, AudioSrc) -> Cmd msg

port getAsset : String -> Cmd msg

port loadedTrack : ((NodeId, AudioSrc) -> msg) -> Sub msg

port finishedTrack : ((NodeId, AudioSrc) -> msg) -> Sub msg


main = text "" 
