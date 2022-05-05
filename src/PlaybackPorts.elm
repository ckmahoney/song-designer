port module PlaybackPorts exposing (playMusic, pauseMusic, stopMusic, setSource, setAndPlaySource, kill, createSource, createAndPlaySource, getAsset, loadedTrack, NodeId, AudioSrc)

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

port loadedTrack : (String -> msg) -> Sub msg


main = text "" 
