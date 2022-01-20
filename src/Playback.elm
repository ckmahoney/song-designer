port module Playback exposing (..)

import Html exposing (Html, div, text)
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

port playMusic : String -> Cmd msg

port pauseMusic : String -> Cmd msg

port stopMusic : String -> Cmd msg

port setSource : String -> Cmd msg

port createSource : (String, String) -> Cmd msg

port getAsset : String -> Cmd msg


type Model
 = Playing 
 | Paused
 | Stopped 


type Asset
  = MixHigh
  | MixLow
  | Stems
  | Midi


type Msg
  = Load (String, String)
  | Play 
  | Pause
  | Stop
  | Select (Maybe TrackMeta)


type alias Player = 
  (Maybe TrackMeta, Model)


new : Player
new = 
  (Nothing, Stopped)


assetName : Asset -> String
assetName kind =
  case kind of 
    MixLow -> 
      "mix-mp3"
  
    MixHigh -> 
      "mix-aiff"
  
    Stems -> 
      "stem-pack"
  
    Midi -> 
      "midi-pack"


trigger : Msg -> Cmd msg
trigger msg =
  case msg of 
    Load (nodeId, filepath) -> createSource (nodeId, filepath)
    Select Nothing -> setSource ""
    Select (Just track) -> createSource ("#the-player", track.filepath)
    Play -> playMusic ""
    Pause -> pauseMusic ""
    Stop -> stopMusic ""


apply : Msg -> Player -> Player
apply msg ((track, model) as p) =
  case msg of  
    Select Nothing -> (Nothing, Stopped)
    Select next -> (next, Paused)
    Play ->  (track, Playing)
    Pause -> (track, Paused)
    Stop -> (track, Stopped)
    _ -> p


update : Msg -> Player -> (Player, Msg)
update msg model =
  (apply msg model, msg)


controls  : Model -> (Msg -> msg) -> Html msg
controls model trig = 
  div [Attr.class "is-flex is-justify-content-space-around"] [ case model of 
      Playing -> 
        div [onClick <| trig Pause] [Components.svg "pause"] 

      Paused -> 
        div [onClick <| trig Play] [Components.svg "play"] 

      Stopped ->
        div [onClick <| trig Play] [Components.svg "play"] 

  , case model of 
      Stopped ->
        text ""
      _ -> div [onClick <| trig Stop] [Components.svg "stop"]
  ]


-- A button to change current selection and begin playback immediately
selectPlay  : Model -> (Msg -> msg) -> Html msg
selectPlay model trig = 
  div [ Attr.class "is-flex is-justify-content-space-around"]  
      [ div [onClick <| trig Play] [Components.svg "play"] 
      ]


assets : TrackMeta -> (Asset -> msg) -> Html msg
assets track req  =
  Components.box
   [ Components.label <| (String.fromInt track.size_bytes) ++ " bytes"
   , Components.header "Downloads"
   , Components.colsMulti
       [ Components.colHalf <| div [] 
           [ Html.button [onClick (req MixLow)] [text "Mixdown mp3"]  
           , Html.p [] [text "Low resolution master recording" ]
           ] 
       , Components.colHalf <| div [] 
           [ Html.button [onClick (req MixHigh)] [text "Mixdown aiff"]
           , Html.p [] [text "Original high resolution master recording" ]
           ]
       -- , Components.colHalf <| div [] 
           -- [ Html.button [onClick (req Midi)] [text "MIDI Stems"]
           -- , Html.p [] [text "All the stems in MIDI format"]
           -- ]
       -- , Components.colHalf <| div [] 
           -- [ Html.button [] [text "Sheet Music"]
           -- , Html.p [] [text "Sheet Music in PDF"]
           -- ]
       -- , Components.colHalf <| div [] 
           -- [ Html.button [] [text "Stem Pack mp3"]
           -- , Html.p [] [text "Recordings required to create the mixdown, in low resolution format"]
           -- ]
       , Components.colSize "is-full" <| div [] 
           [ Html.button [onClick (req Stems)] [text "Stem Pack aiff"]
           , Html.p [] [text "All the recordings required to create the mixdown, in original high resolution format"]
           ]
       ]
   ]


meta track =
  Components.box <|List.singleton <| Components.colsMulti 
    [ Components.colFull <| Components.header "Metadata"
    , Components.colHalf <| Components.label <| View.timeString track.duration_seconds
    , Components.colHalf <| Components.label <| (String.fromInt track.size_bytes) ++ " bytes"
    ]


-- the mini player
face track model trig =
  Components.box
    [ Components.cols 
       [ Components.colHalf <| Components.label "Artist"
       , Components.colHalf <| Components.label track.title
       ]
       , div [Attr.id "the-player"] [] 
       , Components.cols
       [ Components.colHalf <| Components.label "0:00"
       , Components.colHalf <| Components.label <| View.timeString track.duration_seconds
       ]
    , controls model trig
    ]


feature : TrackMeta -> Model ->  (Msg -> msg) -> (Int -> Asset -> msg) -> Html msg 
feature track model trig req =
  div []
    [ face track model trig 
    , meta track
    , assets track (req track.id)
    ]


player : TrackMeta -> Model ->  (Msg -> msg) -> Html msg 
player track model trig =
  face track model trig 


card : Player -> ((Player, Msg) -> msg)-> TrackMeta -> Html msg
card ((selection, model) as p) signal track = 
  let 
    change = (\msg -> signal <| update msg (Just track, model))
    children = case selection of 
      Nothing -> 
         div [onClick <| change <| Select (Just track)] [Components.svg "play"]
      Just selected ->  
        if selected == track then 
          controls model change
        else 
          div [onClick <| change <| Select (Just track)] [Components.svg "play"]
  in
  Components.col [ Attr.class "is-half"] [ Components.songCard track.title <| List.singleton children]


listing : Player -> ((Player, Msg) -> msg)-> TrackMeta -> (String -> msg) -> Html msg
listing ((selection, model) as p) signal track download = 
  let 
    change = (\msg -> signal <| update msg (Just track, model))
    children = case selection of 
      Nothing -> 
         div [onClick <| change <| Select (Just track)] [Components.svg "play"]
      Just selected ->  
        if selected == track then 
          selectPlay model change
        else 
          div [onClick <| change <| Select (Just track)] [Components.svg "play"]

  in
  Components.colSize "is-full"
    <| Components.colsWith [Attr.class "is-vcentered"]
       [ Components.col1 <| Components.label track.title 
       , Components.col1 <| selectPlay model change
       , Components.col1 <| Components.button (download track.filepath) [] "Download"
       ]


playlist : Player -> ((Player, Msg) -> msg) ->  List TrackMeta -> Html msg
playlist  ((selection, model) as p) signal tracks =
  div [] 
   [ Html.h2 [Attr.class "title"] [text "My Songs"]
   , Components.box <| List.singleton  <| Components.colsMulti <|
   List.map (card p signal) tracks
   ] 


actionlist : Player -> ((Player, Msg) -> msg) ->  List TrackMeta -> (String -> msg) -> Html msg
actionlist  ((selection, model) as p) signal tracks download =
  Components.box <| List.singleton  <| Components.colsMulti <|
    List.map ((\x -> listing p signal x download)) tracks


minilist : Player -> ((Player, Msg) -> msg) ->  List TrackMeta -> Html msg
minilist  ((selection, model) as p) signal tracks =
  Components.box <| List.singleton <| Components.colsMulti <|
   List.map (card p signal) tracks


clear : (Player, Msg)
clear  =
  ((apply (Select Nothing) (Nothing, Stopped)), Select Nothing)


view : Player -> ((Player, Msg) -> msg) -> (Int -> Asset -> msg) -> List TrackMeta -> Html msg
view ((selection, model) as p) signal req tracks =
  case selection of 
   Nothing ->
    Components.cols
      [ Components.col1 <| playlist p signal tracks
      ] 

   Just track ->
    let
       change = (\msg -> signal <| update msg (Just track, model))
    in
    Components.cols
      [ Components.colSize "is-one-third" <| feature track model change req
      , Components.colSize "is-two-thirds" <| playlist p signal tracks
      ] 


mini : Player -> ((Player, Msg) -> msg) -> List TrackMeta -> (String -> msg) -> Html msg
mini ((selection, model) as p) signal tracks download =
  case selection of 
   Nothing ->
    text ""

   Just track ->
    let
       change = (\msg -> signal <| update msg (Just track, model))
    in
    Components.cols
      [ Components.colSize "is-one-half" <| player track model change
      , Components.colSize "is-two-thirds" <| actionlist p signal tracks download
      ] 


main = Html.text ""
