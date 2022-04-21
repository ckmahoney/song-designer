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


init : Maybe Int -> (Model a, Cmd msg)
init x =
  (new, Cmd.none)



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


update : Msg -> Model a -> (Model a, Cmd msg)
update msg state = 
  case msg of 
    _ -> (state, Cmd.none)


show : (a -> Html msg) -> List a -> Html msg
show thumb things =
  div [] <| List.map thumb things

view : Model a -> (a -> Html msg) -> Html msg
view model thumb = 
  case model of 
    (Nothing, children) -> show thumb children
    (Just index, children) -> show thumb children


main = text ""


-- main = Browser.element 
--   { init = init
--   , update = update
--   , view = view
--   , subscriptions = (\_ -> Sub.none)
--   }
