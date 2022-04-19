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
type alias Tempo = Float

useSharps = False

type Style
  = Beat
  | Groove
  | Mix
  | Instrumental
  | Abstract

type alias Item =
  { title : String
  , cps : Tempo
  , key : Key
  , style : Style
  }

type Msg 
  = InitEdits
  | SaveEdits
  | KillEdits  

  | SetTitle String
  | SetSize Int
  | SetTempo Float
  | SetKey Int
  | SetStyle Style


type Model 
  = Viewing Item 
  | Editing Item Item

cpsMin = 11/15
cpsMax = 90/15

new : Item
new = 
  Item "New Arc" 1.9 -2 Mix


new2 : Item
new2 = 
  Item "New Arc 2" 2.8 -1 Beat


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
    SetSize size -> item
    SetTempo cps -> item
    SetKey index -> item
    SetStyle style -> item
    _ -> item

    -- KillEdits _ 

update : Msg -> Model -> (Model, Cmd msg) 
update msg state =
  case state of 
    Editing item next -> 
      case msg of 
        SaveEdits -> (Viewing next, Cmd.none)
        _ -> (Editing item <| edit msg next, Cmd.none)

    Viewing model -> 
      case msg of 
       InitEdits -> (Editing model model, Cmd.none)
       _ -> (Viewing model, Cmd.none)


view : Model -> msg -> msg -> (String -> msg) -> (Float -> msg) -> (Int -> msg) -> (Style -> msg) -> Html msg
view model start finish xTitle xTempo xKey xStyle =
  let keys = [Beat, Groove, Mix, Instrumental, Abstract ] 
      select = (\i -> xStyle <| Tools.getOr i keys Mix) 
  in
  case model of 
    Viewing item -> 
      Components.box 
        [ Components.editText "Title" (text "") item.title xTitle
        , Components.editRange "BPM" (text "") (cpsMin, cpsMax) item.cps xTempo
        , Components.keyPicker useSharps item.key xKey
        , text <| Components.keyMessage useSharps item.key
        , Components.button start [] "Edit Arc"
        ]
    Editing orig next ->
      Components.box 
        [ Components.pickerSelected keys (text << styleLabel) select next.style
        , Components.button finish  [] "Save Arc"
        ]


init : Maybe Int -> (Model, Cmd msg)
init flag = 
  (Viewing new, Cmd.none)


main = 
  Browser.element { init = init
                  , update = update
                  , view = (\model -> view model InitEdits SaveEdits SetTitle SetTempo SetKey SetStyle)
                  , subscriptions = (\_ -> Sub.none)
                  }
