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


type Msg a
  = View 
  | InsertAt Index a 
  | Delete Index


new : Model a
new = 
  (Nothing, [])


from : List a -> Model a 
from things =
  (Nothing, things)


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


apply : (Msg a) -> (Model a) -> Model a
apply msg ((mIndex, children) as model) = 
  case msg of
    View -> (Just -1, children)
    InsertAt index el ->
      let
        head = List.take index children
        tail = List.drop index children
        list = head ++ [ el ] ++ tail
      in 
      (Just -1, list)
    Delete index -> 
        (Just -1, Tools.removeAt index children)


update : (Msg a) -> Model a -> (Model a, Cmd msg)
update msg state = 
  case msg of 
    _ -> (state, Cmd.none)


show : (a -> Html msg) -> List a -> Html msg
show thumb things =
  div [] <| List.map thumb things


overview : msg -> (a -> Html msg) -> List a -> Html msg
overview create thumb things =
  div [] <| (List.map thumb things ++ [(Components.button create [] "+")])


view : Model a -> (a -> Html msg) -> (Msg a -> msg) -> msg -> Html msg
view model thumb revise create = 
  case model of 
    (Nothing, children) -> overview create thumb children
    (Just index, children) -> show thumb children


main = text ""


-- main = Browser.element 
--   { init = init
--   , update = update
--   , view = view
--   , subscriptions = (\_ -> Sub.none)
--   }
