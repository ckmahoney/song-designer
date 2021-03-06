module Comm.Playback exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Defs.Types exposing (TrackMeta)
import Defs.Data
import Components.View as View 
import Components.Components as Components
import Tools
import Array
import Http
import Url.Builder as Url
import Json.Decode as Decode
import Json.Encode as Encode
import Defs.Configs as Conf
import Comm.PlaybackPorts exposing (..)


type alias NodeId = String
type alias AudioSrc = String
type alias NoArgs = String



patreonLink =
  "https://patreon.com/cortlandmahoney"


type State
 = Playing 
 | Paused
 | Stopped 
 | Empty


type Asset
  = MixHigh
  | MixLow
  | Stems
  | Midi


type Msg
  = Cue String
  | Load String
  | Play 
  | Pause
  | Stop
  | Select (Maybe TrackMeta)
  | CreateAndPlay (NodeId, AudioSrc)


type alias Model = 
  (Maybe TrackMeta, State)


new : Model
new = 
  (Nothing, Empty)


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


-- Executes sidefx for audio controls
trigger : Msg -> Cmd msg
trigger msg =
  case msg of 
    Cue filepath -> setSource ("#the-player", filepath)
    Load filepath -> setAndPlaySource ("#the-player", filepath)
    Select Nothing -> kill 0
    Select (Just track) -> createSource ("#the-player", track.filepath)
    Play -> playMusic ""
    Pause -> pauseMusic ""
    Stop -> stopMusic ""
    CreateAndPlay (nodeId, src) -> createAndPlaySource (nodeId, src)


apply : Msg -> Model -> Model
apply msg ((track, model) as p) =
  case msg of  
    Select Nothing -> new
    Play ->  (track, Playing)
    Pause -> (track, Paused)
    Stop -> (track, Stopped)
    Cue src -> (track, Stopped)
    Load src -> (track, Playing)
    Select next -> (next, Stopped)
    CreateAndPlay (nodeId, src) -> (track, Playing)



update : Msg -> Model -> (Model, Msg)
update msg model =
  (apply msg model, msg)


controls  : State -> TrackMeta -> (Msg -> msg) -> Html msg
controls state track trig = 
  div [Attr.class "is-flex is-justify-content-space-around"] [ case state of 
    Playing ->
      div [onClick <| trig Pause] [Components.svg "pause"] 

    Paused ->
      div [onClick <| trig Play] [Components.svg "play"] 

    Stopped ->
      div [onClick <| trig <| Load track.filepath] [Components.svg "play"]

    Empty ->
      emptyMessage

  , case state of 
      Stopped ->
        text ""

      _ -> div [onClick <| trig Stop] [Components.svg "stop"]
  ]


-- A button to change current selection and begin playback immediately
selectPlay : TrackMeta -> (Msg -> msg) -> Html msg
selectPlay track trig = 
  div [ Attr.class "is-flex is-justify-content-space-around"]  
      [ div [onClick <| trig (Load track.filepath)] [Components.svg "play"] 
      ]


assets : TrackMeta -> (Asset -> msg) -> Html msg
assets track req  =
  Components.box
   [ Components.label <| (String.fromInt track.size_bytes) ++ " bytes"
   , Components.header "Downloads"
   , Components.colsMulti
       [ Components.colHalf <| div [] 
           [ Html.button [onClick (req MixLow), Attr.class "is-success"] [text "Mixdown mp3"]  
           , Html.p [] [text "Low resolution master recording" ]
           ] 
       , Components.colHalf <| div [] 
           [ Html.button [onClick (req MixHigh), Attr.class "is-success"] [text "Mixdown aiff"]
           , Html.p [] [text "Original high resolution master recording" ]
           ]
       , Components.colHalf <| div [] 
           [ Components.buttonDisabled [Attr.class "is-success"] "MIDI Stems"
           , Html.p [] [text "All the stems in MIDI format"]
           , Html.p [] [text "Coming in version 0.5.0!"]
           , Html.p [] [ text "Do you want MIDI stems sooner? Please support me "
                       , Html.a [Attr.href patreonLink] [text "on Patreon."]
                       , text "It would really help a lot. Thank you!" 
                       ]
           ]
       , Components.colHalf <| div [] 
           [ Components.buttonDisabled [Attr.class "is-success"] "Sheet Music"
           , Html.p [] [text "Sheet Music in PDF"]
           , Html.p [] [text "Coming in version 0.6.0!"]
           , Html.p [] [ text "Do you want Sheet Music sooner? Please support me "
                       , Html.a [Attr.href patreonLink] [text "on Patreon."]
                       , text "It would really help a lot. Thank you!" 
                       ]
           ]
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


meta : TrackMeta -> Html msg
meta track =
  Components.box <|List.singleton <| Components.colsMulti 
    [ Components.colFull <| Components.header "Metadata"
    , Components.colHalf <| Components.label <| View.timeString track.duration_seconds
    , Components.colHalf <| Components.label <| (String.fromInt track.size_bytes) ++ " bytes"
    ]


-- the mini player
face : TrackMeta -> State ->  (Msg -> msg) -> Html msg 
face track state trig =
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
    , controls state track trig
    ]


feature : TrackMeta -> State ->  (Msg -> msg) -> (Int -> Asset -> msg) -> Html msg 
feature track state trig req =
  div []
    [ face track state trig 
    , meta track
    , assets track (req track.id)
    ]


player : TrackMeta -> State ->  (Msg -> msg) -> Html msg 
player track state trig =
  face track state trig 


card : Model -> ((Model, Msg) -> msg)-> TrackMeta -> Html msg
card ((selection, state) as p) signal track = 
  let 
    change = (\msg -> signal <| update msg (Just track, state))
    children = case selection of 
      Nothing -> 
         div [onClick <| change <| Load track.filepath] [Components.svg "play"]
      Just selected ->  
        if selected == track then 
          controls state track change
        else 
          div [onClick <| change <| Load track.filepath] [Components.svg "play"]
  in
  Components.col [ Attr.class "is-half"] [ Components.songCard track.title <| List.singleton children]


listing : Model -> ((Model, Msg) -> msg)-> TrackMeta -> (String -> msg) -> Html msg
listing ((selection, state) as p) signal track download = 
  let 
    change = (\msg -> signal <| update msg (Just track, state))
    children = case selection of 
      Nothing -> 
         div [onClick <| change <| Load track.filepath] [Components.svg "play"]
      Just selected ->  
        if selected == track then 
          selectPlay track change
        else 
          div [onClick <| change <| Load track.filepath] [Components.svg "play"]

  in
  Components.colSize "is-full"
    <| Components.colsWith [Attr.class "is-vcentered"]
       [ Components.col1 <| Components.label track.title 
       , Components.col1 <| selectPlay track change
       , Components.col1 <| Components.button (download track.filepath) [] "Download"
       ]


playlist : Model -> ((Model, Msg) -> msg) ->  List TrackMeta -> (String -> msg) -> Html msg
playlist  ((selection, model) as p) signal tracks download =
  div [] 
   [ Html.h2 [Attr.class "title"] [text "My Songs"]
   , Components.box <| List.singleton  <|
       actionlist p signal tracks download
   ] 


actionlist : Model -> ((Model, Msg) -> msg) ->  List TrackMeta -> (String -> msg) -> Html msg
actionlist  ((selection, model) as p) signal tracks download =
  Components.box <| List.singleton  <| Components.colsMulti <|
    List.map ((\x -> listing p signal x download)) tracks


minilist : Model -> ((Model, Msg) -> msg) ->  List TrackMeta -> Html msg
minilist  ((selection, model) as p) signal tracks =
  Components.box <| List.singleton <| Components.colsMulti <|
   List.map (card p signal) tracks


clear : (Model, Msg)
clear  =
  ((apply (Select Nothing) (Nothing, Stopped)), Select Nothing)


view : Model -> ((Model, Msg) -> msg) -> (Int -> Asset -> msg) -> List TrackMeta -> (String -> msg) -> Html msg
view ((selection, model) as p) signal req tracks download =
  case selection of 
   Nothing ->
    Components.cols
      [ Components.col1 <| playlist p signal tracks download
      ] 

   Just track ->
    let
       change = (\msg -> signal <| update msg (Just track, model))
    in
    Components.cols
      [ Components.colSize "is-one-third" <| feature track model change req
      , Components.colSize "is-two-thirds" <| playlist p signal tracks download
      ] 


mini : Model -> ((Model, Msg) -> msg) -> List TrackMeta -> (String -> msg) -> Html msg
mini ((selection, model) as p) signal tracks download =
  case selection of 
   Nothing ->
    if List.length tracks > 0 then 
      Components.box [ emptyMessage ]
    else text ""


   Just track ->
    let
       change = (\msg -> signal <| update msg p)
    in
    div [Attr.id "mini-player"] <| List.singleton <|
     Components.cols
      [ Components.colSize "is-one-half" <| face track model change
      , Components.colSize "is-two-thirds" <| actionlist p signal tracks download
      ] 


bottomPlayer : (Maybe TrackMeta) -> Html msg -> Html msg
bottomPlayer selection audio =
  let
    visiblity = case selection of 
      Nothing -> "hidden"
      Just track -> "visible"
  in 
  div [ Attr.class "bottom-player", Attr.class visiblity ] 
   [ Components.label "artist"
   , Components.label "title"
   , Components.buttonDisabled [] "Options"
   , Components.buttonDisabled [] "Volume"
   , Components.buttonDisabled [] "PlayPause"
   , audio
   , text "3:24"
   ]


emptyMessage : Html msg
emptyMessage =
  Components.paragraph "Select any song to start playing."


main = Html.text ""
