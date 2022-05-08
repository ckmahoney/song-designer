port module Comm.PlaybackPorts exposing (..)

import Html exposing (text)


type alias NodeId = String
type alias AudioSrc = String
type alias NoArgs = Int


port playMusic : NodeId -> Cmd msg

port pauseMusic : NodeId -> Cmd msg

port stopMusic : NodeId -> Cmd msg

port setSource : (NodeId, AudioSrc) -> Cmd msg

port setAndPlaySource : (NodeId, AudioSrc) -> Cmd msg

port kill : NoArgs -> Cmd msg

port createSource : (NodeId, AudioSrc) -> Cmd msg

port createAndPlaySource : (NodeId, AudioSrc) -> Cmd msg

port getAsset : String -> Cmd msg

port loadedTrack : ((NodeId, AudioSrc) -> msg) -> Sub msg

port finishedTrack : ((NodeId, AudioSrc) -> msg) -> Sub msg


main = text "" 
