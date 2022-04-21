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

type alias MetaModel =
  { title : String
  , tempo : String
  , bpm : Bpm
  , key : Key
  , style : Style
  }


type alias Model =
  { title : String
  , size : Int
  , key : Key
  , style : Style
  }


type Msg 
  = InitEdits
  | SaveEdits
  | KillEdits  

  | SetTitle String
  | SetSize Int
  | SetKey Int
  | SetStyle Style


type State 
  = Viewing Model 
  | Editing Model Model

bpmMin = 44.0
bpmMax = 360.0

sizeMin = 3
sizeMax = 5

new : Model
new = 
  Model "Verse" 4 -2 Mix


new2 : Model
new2 = 
  Model "Chorus" 5 -1 Groove 

new3 : Model
new3 = 
  Model "Break" 3 1 Beat


styleLabel : Style -> String
styleLabel style = 
  case style of 
    Beat -> "Beat"
    Groove -> "Groove"
    Mix -> "Mix"
    Instrumental -> "Instrumental"
    Abstract -> "Abstract"

sizeLabel : Int -> String
sizeLabel size = 
  case size of 
    4 -> "Short"
    5 -> "Medium"
    6 -> "Long"
    _ -> "? mystery size ?"
    


styleInfo : Style -> String
styleInfo style = 
  case style of
    Beat -> "Just percussive parts, less clear harmony or melody"
    Groove -> "Mostly percussion with some bass or chords"
    Mix -> "All parts evenly balanaced"
    Instrumental -> "Mostly instruments with kick or hats"
    Abstract -> "Just instruments, less clear rhythms"

edit : Msg -> Model -> Model
edit msg item = 
  case msg of 
    SetTitle title -> {item | title = title}
    SetSize size -> { item | size = size } 
    SetKey index -> { item | key = index }
    SetStyle style -> { item| style = style }
    _ -> item

     -- SetBpm let 
     --    c = Maybe.withDefault item.size <| String.toInt t
     --    size = if c < sizeMin then sizeMin else if c > sizeMax then sizeMax else c
     --    display = if "" == t then "" else t
     -- in 
     --  { item | sizeMessage = t, size = size }


apply : Msg -> State -> State
apply msg state =
  case state of 
    Editing orig next -> 
      case msg of 
        SaveEdits -> Viewing next
        KillEdits -> Viewing orig
        _ -> Editing orig <| edit msg next

    Viewing item -> 
      case msg of 
       InitEdits -> Editing item item
       _ -> Viewing item


update : Msg -> State -> (State, Cmd msg) 
update msg state =
  (apply msg state, Cmd.none)


stub : Model -> msg -> Html msg
stub { title, key, style, size } click = 
  Components.box
   [ Components.label title
   , Components.button click [] "Edit Arc"
   -- , text <| View.keyLabel key
   -- , text <| (styleLabel style) ++ (styleInfo style)
   -- , text <| View.bpmString bpm
 
   ]


sizes : List Int
sizes = [2, 3, 4]


view : State -> msg -> msg -> msg -> (String -> msg) -> (Int -> msg) -> (Int -> msg) -> (Style -> msg) -> Html msg
view model start finish cancel xTitle xSize xKey xStyle =
  let keys = [Beat, Groove, Mix, Instrumental, Abstract ] 
      select = (\i -> xStyle <| Tools.getOr i keys Mix) 
      selectSize = (\i -> xSize <| Tools.getOr i sizes 6)
  in
  case model of 
    Viewing item -> 
      Components.box 
        [ stub item start
        ]

    Editing orig next ->
      Components.box 
        [ Components.editText "Title" (text "") next.title xTitle
        , Components.pickerSelected sizes (text << sizeLabel) selectSize next.size
        , Components.keyPickerFull useSharps next.key xKey
        , Components.pickerSelected keys (text << styleLabel) select next.style
        , Components.button finish  [] "Save Arc"
        , Components.button cancel  [] "Cancel Changes"
        ]


initState : State
initState = Editing new new


init : Maybe Int -> (State, Cmd msg)
init flag = 
  -- (Viewing new, Cmd.none)
  (initState, Cmd.none)

viewSlim : State -> msg -> msg -> msg -> Html msg
viewSlim state start finish cancel = 
  case state of 
    Viewing item -> 
      Components.box 
        [ stub item start
        ]

    Editing orig next ->
      Components.box 
        [ text next.title
        , Components.button finish  [] "Save Arc"
        , Components.button cancel  [] "Cancel Changes"
        ]
  

main = 
  Browser.element { init = init
                  , update = update
                  , view = (\model -> view model InitEdits SaveEdits KillEdits SetTitle SetSize SetKey SetStyle)
                  , subscriptions = (\_ -> Sub.none)
                  }
