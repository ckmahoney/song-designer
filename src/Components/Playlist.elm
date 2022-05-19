module Components.Playlist exposing (..)

import Html exposing (Html, div, text, p)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Defs.Types exposing (TrackMeta)
import Defs.Data as Data
import Defs.Configs as Conf
import Components.Components as Components
import Components.Player as Player
import Tools

type alias Model = 
  (List Player.Model, List TrackMeta)


type Msg 
  = Change Int Player.Msg
  | Add TrackMeta
  | AddMany (List TrackMeta)


id : Int -> String
id dbId =
  "player-" ++ String.fromInt dbId


apply : Msg -> Model -> Model
apply msg (players, tracks) =
  case msg of 
    Add track ->  
      let 
        player = Player.new (id <| Debug.log "Adding a track with id" track.id)
      in 
      (player :: players, track :: tracks)

    AddMany moreTracks -> 
      let 
        size = List.length tracks 
        morePlayers = List.indexedMap (\i track -> 
          Player.new <| id track.id ) moreTracks
      in 
      (List.append players morePlayers, List.append tracks moreTracks)

    _ ->
      (players, tracks)


update : Msg -> Model -> (Model, Cmd msg)
update msg ((players, tracks) as model) =
  case msg of 
    Change index pMsg -> 
      let 
        state = Tools.getOr index players (Player.new (id index))
        player = Player.apply pMsg state
      in 
      ((Tools.replaceAt index player players, tracks), Player.trigger pMsg state)

    _ ->
      (apply msg model, Cmd.none)


new : Model
new =
  ([], [])


listing : Bool -> Model -> (Msg -> msg)-> TrackMeta -> (String -> msg) -> Html msg
listing isAnon ((state, tracks) as model) do track download = 
  let 
    change = (\msg -> do  msg)
    children = 
      div [] [Components.svg "play"]
  in
  Components.colSize "is-full"
    <| Components.colsWith [Attr.class "is-vcentered"]
       [ Components.col1 <| Components.label track.title 
       , if isAnon then 
          Components.buttonDisabled [] "Login to Download"          
         else 
          Components.col1 <| Components.button (download track.filepath) [] "Download"
       ]


regCta : Html msg
regCta = 
  Html.p [Attr.class " mb-3"] [ text "To download these songs and keep them as your own, log into your account. Every song you make while logged is yours. You can anything you want with it! We do not claim any rights, license, or ownership over the songs you make." ]


termsCta : Html msg
termsCta =
  Html.p [] [ text "Those terms are described in more detail ", Html.a [Attr.href <| Conf.selfUrl "terms-of-service" ] [ text "here" ], text "." ]


playlist : Bool -> Model -> (String -> msg) -> Html msg
playlist isAnon model download =
  Components.box
   [ Html.h2 [Attr.class "title mt-6"] [text "My Music"] 
   , if isAnon then regCta else text ""
   , if isAnon then termsCta else text ""
   ]


-- instance of card when there is only 1 allowed waveform
card0 : Player.State -> (Player.Msg -> msg) -> Bool -> TrackMeta -> Html msg
card0 state change isSelected ({filepath, title} as track) =
  let
    control = 
     if isSelected then 
      case state of 
        Player.Playing _ ->
          Components.button (change Player.Pause) [] "Pause"
        
        Player.Loading _ ->
          Components.button (change Player.Stop) [] "Cancel"
        
        _ -> 
          Components.button (change <| Player.Play) [] "Play" 
     else
      Components.button (change <| Player.Load track) [] "Play" 

    buttons = 
      [ control
      , Components.buttonDisabled []  "More"
      , Components.buttonDisabled []  "Hide"
      ]
  in 
  Components.col1 <| Components.card3 (text title) (text "") buttons 

-- instance of card with arbitrary number of waveforms
card : Bool -> Int -> Player.State -> (Int -> Player.Msg -> msg) ->  TrackMeta -> (String -> msg) -> Html msg
card isAnon index state change ({filepath, title} as track) download =
  let
    control = 
      case state of 
        Player.Playing _ ->
          Components.button (change index Player.Pause) [] "Pause"
        
        Player.Paused _ ->
          Components.button (change index Player.Play) [] "Play"
        
        Player.Loading _ ->
          Components.button (change index Player.Stop) [] "Cancel"
        
        _ -> 
          Components.button (change index <| Player.Load track) [] "Play"
    
    body = div []
      [ div [ Attr.id (id track.id) ] [] 
      , case state of 
          Player.Loading _ -> 
            text "Loading your track"
          _ ->
            p [ Attr.class "content has-text-centered" ] [ text <| Tools.timeString track.duration_seconds ]
      ]

    dlButton = if isAnon then 
       Components.buttonDisabled [] "Download"          
      else 
       Components.col1 <| Components.button (download track.filepath) [] "Download"

    buttons = 
      [ control
      , dlButton
      ]
  in 
  div [Attr.class "column is-half"] [ Components.card3 (text title) body buttons ]



view : Bool -> Model -> (Msg -> msg) ->  (String -> msg) -> (Int -> Player.Msg -> msg) -> Html msg
view isAnon ((players, tracks) as model) updatePlaylist download do =
  if List.length tracks > 0 then 
    let pairs = List.map2 Tuple.pair players tracks in 

    div [] <|
      [ Components.cols [ Components.col1 <| playlist isAnon model download ]
      , Html.p [Attr.class "show-on-mobile"] [ text "On mobile? Make sure your mute switch is off and the volume is up." ]
      , Components.colsMulti <| List.indexedMap (\i (state, track) -> card isAnon i (Tuple.second state) do track download) pairs
      ]

  else   
    p [] [ text "Make a song to start your collection." ]


main = Html.text ""
