module Arc exposing (..)

import Browser
import Html exposing (Html, h1, h3, button, div, text, label, p, b)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick)

import View 
import Components
import Tools

type alias Title = String
type alias Key = Int
type alias Tempo = Float -- string representation of the BPM
type alias Bpm = Float

useSharps = False

type Style
  = Beat
  | Groove
  | Mix
  | Instrumental
  | Abstract


type alias Model =
  { title : String
  , size : Int
  , style : Style
  }


type Msg 
  = SetTitle String
  | SetSize Int
  | SetStyle Style


type State 
  = Viewing Model 
  | Editing Model Model


initState : State
initState = Editing new new


init : Maybe Int -> (State, Cmd msg)
init flag = 
  -- (Viewing new, Cmd.none)
  (initState, Cmd.none)


sizeMin = 2
sizeMax = 4

empty : Model
empty = 
  Model "New Arc" 3 Mix

new : Model
new = 
  Model "intro/outro" 2 Beat


new2 : Model
new2 = 
  Model "Verse" 3 Mix

new3 : Model
new3 = 
  Model "Chorus" 2 Groove

new4 : Model
new4 = 
  Model "Break" 2 Abstract

create : Model
create = 
  Model "New Arc" 4 Mix

stateFrom : Model -> State
stateFrom arc = 
  Viewing arc

editArc : Model -> State
editArc arc = 
  Editing arc arc


styleLabel : Style -> String
styleLabel style = 
  case style of 
    Beat -> "Beat"
    Groove -> "Groove"
    Mix -> "Mix"
    Instrumental -> "Instrumental"
    Abstract -> "Synth"


sizeLabel : Int -> String
sizeLabel size = 
  case size of 
    2 -> "Short"
    3 -> "Medium"
    4 -> "Long"
    _ -> "? mystery size ?"


styleInfo : Style -> String
styleInfo style = 
  case style of
    Beat -> "Only percussive parts, like kick drum, hi hats, clap, and snare drum"
    Groove -> "Mostly percussion with some chance of synth parts"
    Mix -> "An equal chance of percussion and synth parts"
    Instrumental -> "Mostly synths with some chance of percussive parts"
    Abstract -> "Only synths, like bass, chords, and melody."


edit : Msg -> Model -> Model
edit msg item = 
  case msg of 
    SetTitle title -> {item | title = title}
    SetSize size -> { item | size = size } 
    SetStyle style -> { item| style = style }


apply : Msg -> State -> State
apply msg state =
  case state of 
    Editing orig next -> 
      case msg of 
        _ -> Editing orig <| edit msg next

    Viewing item -> 
      case msg of 
       _ -> Viewing item


update : Msg -> State -> (State, Cmd msg) 
update msg state =
  (apply msg state, Cmd.none)


stub : Model -> msg -> Html msg
stub { title, style, size } click = 
  div [Attr.class "box", Attr.style "min-width" "240px"]
   [ Components.label title
   , p [ Attr.class "mt-3"] [ text <| sizeLabel size ++ " "  ++ styleLabel style ]
   , Components.button click [class "my-3"] "Edit Arc"
   ]


thumb : Model ->  Html msg
thumb { title, style, size }  = 
  div [Attr.class "box", Attr.style "min-width" "240px"]
   [ Components.label title
   , p [ Attr.class "mt-3"] [ text <| sizeLabel size ++ " "  ++ styleLabel style ]
   ]


sizes : List Int
sizes = [2, 3, 4]


styles : List Style
styles = [Beat, Groove, Mix, Instrumental, Abstract ] 


readonly : Model -> msg -> msg -> Html msg
readonly arc revise done = 
  Components.box
    [ Components.label arc.title
    , Components.button revise [] "Edit"
    , Components.button done [] "Return"
    ]


editor : Model -> (Msg -> msg) -> msg -> msg -> Html msg
editor arc change save cancel = 
  let
    selectStyle = (\int ->
      change <| SetStyle (Tools.getOr int styles Mix))
  in 
  Components.box
    [ Components.editText "Title" (text "") arc.title (\str -> (change <| SetTitle str))
    , h3 [] [ text "Size" ]
    , div [class "my-6"] <| List.singleton <| Components.pickerSelected sizes (text << sizeLabel) (\int -> change <| SetSize <| Tools.getOr int sizes 3) arc.size
    , h3 [] [ text "Mix" ]
    , div [class "my-6"] <| List.singleton <| Components.pickerSelected styles (text << styleLabel) selectStyle arc.style
    , div [class "w-100"] <| List.singleton <| Html.b [class "is-block has-text-centered content" ]  [ text <| styleInfo arc.style ]
    , Components.button save [Attr.class "mr-3"] "Save changes" 
    , Components.button cancel [] "Cancel" 
    ]
  
main = text ""
