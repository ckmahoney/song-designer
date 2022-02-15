module Components.Carousel exposing (..)

import Browser
import Html exposing (Html, button, div, text, label, p, input,b)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onInput)

import Components


type alias Index = Int


type alias Model a = (Index, List a)


type Msg 
  = RotateRight
  | RotateLeft
  | Content


newModel : Model (Html msg)
newModel = 
  (0, someItems)


init : Maybe Int -> (Model (Html msg), Cmd msg)
init x =
  (newModel, Cmd.none)


someItems : List (Html msg)
someItems = 
  [ Components.box <| [Components.label "First thing"]
  , Components.box <| [Components.label "Second thing"]
  , Components.box <| [Components.label "Third thing"]
  ]


apply : Msg -> Model (Html msg) -> Model (Html msg)
apply msg (index, items) = 
  case msg of
    RotateRight -> (index + 1, items)
    RotateLeft -> (index - 1, items)
    _ -> (index, items)


update : Msg -> Model (Html msg) -> (Model (Html msg), Cmd msg)
update msg model = 
  (apply msg model, Cmd.none)


item : Int -> Int -> Int -> Html msg -> Html Msg
item count index offset thing =
  let
    deg = Debug.log "has degree;" <| (toFloat (index + offset)) * (360.0 / (toFloat count))
    rotate = "rotateY(" ++ String.fromFloat deg  ++ "deg) translateZ(150px)"
  in 
  Html.map (\_ -> Content) <| 
    div [Attr.class "item", Attr.style "transform" rotate] [ thing ]


view : (Model (Html msg)) -> Msg -> Msg -> Html Msg
view (index, items) left right = 
  Components.box <|
  [ Components.label <| String.fromInt index
  , Components.cols 
    [ Components.col1 <| Components.button left [] "Left"
    , Components.col1 <| Components.button right [] "Right"
    ]
  , div [Attr.class "carousel"] <| List.indexedMap (\i x -> item (List.length items) index i x) items
  ]


viewTest : Model (Html msg) -> Html Msg
viewTest model =
 let
  left = RotateLeft
  right = RotateRight
 in 
  view model left right


main = Browser.element 
  { init = init
  , update = update
  , view = viewTest
  , subscriptions = (\_ -> Sub.none)
  }
