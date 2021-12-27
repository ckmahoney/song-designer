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


port playMusic : String -> Cmd msg

port pauseMusic : String -> Cmd msg

port stopMusic : String -> Cmd msg

port setSource : String -> Cmd msg


type Model
 = Playing 
 | Paused
 | Stopped 


type Msg
  = Load String
  | Play 
  | Pause
  | Stop
  | Select (Maybe TrackMeta)


type alias Player = 
  (Maybe TrackMeta, Model)


new : Player
new = 
  (Nothing, Stopped)



trigger : Msg -> Cmd msg
trigger msg =
  case msg of 
    Load filepath -> setSource filepath
    Select Nothing -> setSource ""
    Select (Just track) -> setSource track.filepath
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


player : Player -> ((Player, Msg) -> msg) -> msg -> Html msg
player ((track, state) as p) signal exit =
  div [] 
  [ Components.svgButton "close" exit
  , case state of
      Playing ->
        div [onClick <| signal <| update Pause p] [text "playing"]

      Paused ->
        div [onClick <| signal <| update Play p] [text "paused"]

      Stopped ->
        div [onClick <| signal <| update Play p] [text "play"]
  ]


card : Player -> ((Player, Msg) -> msg)-> TrackMeta -> Html msg
card ((selection, model) as p) signal track = 
  let 
    f = (\msg -> signal <| update msg (Just track, model))
    children = case selection of 
      Nothing -> 
         [ div [onClick <| f <| Select (Just track)] [Components.svg "play"] ]
      Just selected ->  
        if selected == track then 
          [ case model of 
              Playing -> 
                div [onClick <| f Pause] [Components.svg "pause"] 

              Paused -> 
                div [onClick <| f Play] [Components.svg "play"] 

              Stopped ->
                div [onClick <| f Play] [Components.svg "play"] 

          , div [onClick <| f Stop] [Components.svg "stop"]
          ]
        else 
          [ div [onClick <| f <| Select (Just track)] [Components.svg "play"] ]
  in
  Components.col [ Attr.class "is-one-half"] [ Components.songCard track.title children]


playlist : Player -> ((Player, Msg) -> msg) ->  List TrackMeta -> Html msg
playlist  ((selection, model) as p) signal tracks =
  Components.box <| List.singleton  <| Components.colsMulti <|
   List.map (card p signal) tracks


clear : (Player, Msg)
clear  =
  ((apply (Select Nothing) (Nothing, Stopped)), Select Nothing)


view : Player -> ((Player, Msg) -> msg) ->  List TrackMeta -> Html msg
view ((selection, model) as p) signal tracks =
  case selection of 
   Nothing ->
    Components.cols
      [ Components.col1 <| playlist p signal tracks
      ] 

   Just track ->
    Components.cols
      [ Components.colSize "is-one-third" <| player p signal (signal clear)
      , Components.colSize "is-two-thirds" <| playlist p signal tracks
      ] 



main = Html.text ""
