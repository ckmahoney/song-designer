module Components.Group exposing (..)

import Browser
import Html exposing (Html, h1, button, div, text, label, p, input,b)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onInput)
import Tools

import Components

type alias Index = Int

type alias Dimensions = (List Float, List Float)

type alias Model a = (Maybe Index, List a)

type alias TType = 
  { name : String
  , age : Int
  }

type State a 
  = Viewing (Model a)
  | Editing (Model a) a
  
type Msg 
  = View 
  | Edit


new : Model a
new = 
  (Just -1, [])


from : List a -> Model a 
from things =
  (Just -1, things)

by : Int -> List a -> Model a
by index things =
  (Just index, things)


init : Maybe Int -> (State a, Cmd msg)
init x =
  (Viewing new, Cmd.none)

stateFrom : Model a -> State a
stateFrom model =
  Viewing model
  


someItems : List (Html msg)
someItems = 
  [ Components.box <| [Components.label "A"]
  , Components.box <| [Components.label "B"]
  , Components.box <| [Components.label "C"]
  , Components.box <| [Components.label "D"]
  , Components.box <| [Components.label "E"]
  ]


newModel : Model (Html msg)
newModel = 
 let 
  zeros = (List.map (\_ ->  0.0) someItems)
 in 
 (Nothing, [])


apply : Msg -> (Model a) -> Model a
apply msg ((mIndex, children) as model) = 
  case msg of
    _ -> model


update : Msg -> State a -> (State a, Cmd msg)
update msg state = 
  case msg of 
    _ -> (state, Cmd.none)


view : State a -> (a -> Html msg) -> Html msg
view state thumb = 
  case state of 
    Viewing (mIndex, children) ->
      Components.box <|
      [ h1 [] [text "Group"] ] ++
      List.map thumb children

    Editing _ _ -> 
      text "Edit this thing"

main = text ""


-- main = Browser.element 
--   { init = init
--   , update = update
--   , view = view
--   , subscriptions = (\_ -> Sub.none)
--   }
