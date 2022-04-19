module Card exposing (..)

import Browser
import Html exposing (Html, h1, button, div, text, label, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Types exposing (..)
import Data
import View 
import Components
import Tools
import Http
import Url.Builder as Url
import Json.Decode as Decode
import Json.Encode as Encode
import Encoders as JE
import Configs as Conf

import ScopeEditor
import EnsembleEditor
import ComboEditor
import LayoutEditor
import Playback

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

type alias Item =
  { title : String
  , tempo : String
  , bpm : Bpm
  , key : Key
  , style : Style
  }

type Msg 
  = InitEdits
  | SaveEdits
  | KillEdits  

  | SetTitle String
  | SetBpm String
  | SetKey Int
  | SetStyle Style


type Model 
  = Viewing Item 
  | Editing Item Item

bpmMin = 44.0
bpmMax = 360.0

new : Item
new = 
  Item "New Arc" "104" 104 -2 Mix


new2 : Item
new2 = 
  Item "New Arc 2" "144" 144 -1 Beat


styleLabel : Style -> String
styleLabel style = 
  case style of 
    Beat -> "Beat"
    Groove -> "Groove"
    Mix -> "Mix"
    Instrumental -> "Instrumental"
    Abstract -> "Abstract"


styleInfo : Style -> String
styleInfo style = 
  case style of
    Beat -> "Just percussive parts, less clear harmony or melody"
    Groove -> "Mostly percussion with some bass or chords"
    Mix -> "All parts evenly balanaced"
    Instrumental -> "Mostly instruments with kick or hats"
    Abstract -> "Just instruments, less clear rhythms"

edit : Msg -> Item -> Item
edit msg item = 
  case msg of 
    SetTitle title -> {item | title = title}
    SetBpm t -> 
     let 
        c = Maybe.withDefault item.bpm <| String.toFloat t
        bpm = if c < bpmMin then bpmMin else if c > bpmMax then bpmMax else c
        display = if "" == t then "0" else t
     in 
      { item | tempo = display, bpm = bpm }
    SetKey index -> { item | key = index }
    SetStyle style -> { item| style = style }
    _ -> item

update : Msg -> Model -> (Model, Cmd msg) 
update msg state =
  case state of 
    Editing orig next -> 
      case msg of 
        SaveEdits -> (Viewing next, Cmd.none)
        KillEdits -> (Viewing orig, Cmd.none)
        _ -> (Editing orig <| edit msg next, Cmd.none)

    Viewing item -> 
      case msg of 
       InitEdits -> (Editing item item, Cmd.none)
       _ -> (Viewing item, Cmd.none)


stub : Item -> Html msg
stub { title, key, style, bpm } = 
  Components.box
   [ Components.label title
   , text <| View.keyLabel key
   , text <| (styleLabel style) ++ (styleInfo style)
   , text <| View.bpmString bpm
   ]


view : Model -> msg -> msg -> msg -> (String -> msg) -> (String -> msg) -> (Int -> msg) -> (Style -> msg) -> Html msg
view model start finish cancel xTitle xBpm xKey xStyle =
  let keys = [Beat, Groove, Mix, Instrumental, Abstract ] 
      select = (\i -> xStyle <| Tools.getOr i keys Mix) 
  in
  case model of 
    Viewing item -> 
      Components.box 
        [ stub item 
        , Components.button start [] "Edit Arc"
        ]

    Editing orig next ->
      Components.box 
        [ Components.editText "Title" (text "") next.title xTitle
        , Components.editRangeString "BPM" (text "") (bpmMin, bpmMax) next.tempo xBpm
        , Components.keyPickerFull useSharps next.key xKey
        , Components.pickerSelected keys (text << styleLabel) select next.style
        , text <|  "Applying bpm :" ++ String.fromFloat next.bpm
        , Components.button finish  [] "Save Arc"
        , Components.button cancel  [] "Cancel Changes"
        ]


init : Maybe Int -> (Model, Cmd msg)
init flag = 
  -- (Viewing new, Cmd.none)
  (Editing new new, Cmd.none)

main = 
  Browser.element { init = init
                  , update = update
                  , view = (\model -> view model InitEdits SaveEdits KillEdits SetTitle SetBpm SetKey SetStyle)
                  , subscriptions = (\_ -> Sub.none)
                  }
