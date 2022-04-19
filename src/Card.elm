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

type Sway
  = Beat
  | Groove
  | Mix
  | Instrumental
  | Abstract

type alias Item =
  { title : String
  , tempo : Tempo
  , key : Key
  , mix : Sway
  }

type Msg 
  = InitEdits Item
  | SaveEdits Item
  | KillEdits Item  

  | SetTitle String
  | SetSize Int
  | SetTempo Float
  | SetKey Float
  | SetEnsemble Sway


type Model 
  = Viewing Item 
  | Editing Item Item

new : Item
new = 
  Item "New Arc" 1.9 -2 Mix


new2 : Item
new2 = 
  Item "New Arc 2" 2.8 -1 Beat


swayStr : Sway -> String
swayStr sway = 
  case sway of 
    Beat -> "Beat"
    Groove -> "Groove"
    Mix -> "Mix"
    Instrumental -> "Instrumental"
    Abstract -> "Abstract"


swayPip : Sway -> String
swayPip sway = 
  case sway of
    Beat -> "Just percussive parts, less clear harmony or melody"
    Groove -> "Mostly percussion with some bass or chords"
    Mix -> "All parts evenly balanaced"
    Instrumental -> "Mostly instruments with kick or hats"
    Abstract -> "Just instruments, less clear rhythms"


update : Msg -> Model -> (Model, Cmd msg) 
update msg model =
  case msg of 
    SetTitle title -> (model, Cmd.none)
    SetSize size -> (model, Cmd.none)
    SetTempo cps -> (model, Cmd.none)
    SetKey index -> (model, Cmd.none)
    SetEnsemble sway -> (model, Cmd.none)
    _ -> (model, Cmd.none)

view : Model -> (String -> msg) -> Html msg
view model xTitle = 
  case model of 
    Viewing item -> 
      Components.box 
        [ Components.editText "Title" (text "") item.title xTitle
        , p [] [text "size"]
        , p [] [text "key"]
        , p [] [text "style"]
        ]
    Editing orig next ->
      text "Editing the thing"


init : Maybe Int -> (Model, Cmd msg)
init flag = 
  (Viewing new, Cmd.none)





main = 
  Browser.element { init = init
                  , update = update
                  , view = (\model -> view model SetTitle)
                  , subscriptions = (\_ -> Sub.none)
                  }
