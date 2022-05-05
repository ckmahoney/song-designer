module Components.Playlist exposing (..)

import Html exposing (Html, div, text, p)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Types exposing (TrackMeta)
import Data
import Configs as Conf
import Components
import Components.Player as Player


type alias Model = 
  (Player.Model, List TrackMeta)


type Msg 
  = Change Player.Msg
  | Add TrackMeta
  | AddMany (List TrackMeta)


apply : Msg -> Model -> Model
apply msg (player, list) =
  case msg of 
    Add track ->
      (player, track :: list)

    AddMany tracks -> 
      (player, List.append list tracks)

    _ ->
      (player, list)


update : Msg -> Model -> (Model, Cmd msg)
update msg ((state, tracks) as model) =
  case msg of 
    Change pMsg -> 
      ((Player.apply pMsg state, tracks), Player.trigger pMsg state)

    _ ->
      (apply msg model, Cmd.none)


new : Model
new =
  (Player.default, [])


test : Model
test =
  (Player.default, Data.someTracks)


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
          Components.buttonDisabled [] "Download"          
         else 
          Components.col1 <| Components.button (download track.filepath) [] "Download"
       ]


regCta : Html msg
regCta = 
  Html.p [Attr.class " mb-3"] [ text "To download these songs and keep them as your own, log into your account. Every song you make while logged is yours. You can anything you want with it! We do not claim any rights, license, or ownership over the songs you make." ]


termsCta : Html msg
termsCta =
  Html.p [] [ text "Those terms are described in more detail ", Html.a [Attr.href <| Conf.selfUrl "terms-of-service" ] [ text "here" ], text "." ]


playlist : Bool -> Model -> (Msg -> msg) -> (String -> msg) -> Html msg
playlist isAnon model do download =
  div [] 
   [ Html.h2 [Attr.class "title mt-6"] [text "My Music"] 
   , regCta
   , termsCta
   ]

card : TrackMeta -> Html msg
card ({filepath, title} as track) =
  let
    buttons = 
      [ Components.buttonDisabled []  "Play" 
      , Components.buttonDisabled []  "More"
      , Components.buttonDisabled []  "Hide"
      ]
  in 
  Components.col1 <| Components.card3 (text title) (text "") buttons 


view : Bool -> Model -> (Msg -> msg) ->  (String -> msg) -> Html msg
view isAnon ((state, tracks) as model) do download =
    if List.length tracks > 0 then 
      div [] <|
        [ Components.cols [ Components.col1 <| playlist isAnon model do download ]
        , Html.p [Attr.class "show-on-mobile"] [ text "On mobile? Make sure your mute switch is off and the volume is up." ]
        , Components.colsMulti (List.map card tracks)
        ] 
        

    else   
      p [] [ text "Make a song to start your collection." ]


main = Html.text ""
