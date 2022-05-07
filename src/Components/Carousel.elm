module Components.Carousel exposing (..)

import Browser
import Browser.Dom
import Html exposing (Html, button, div, text, label, p, input,b)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onInput)
import Task
import Tools

import Components.Components as Components


type alias Index = Int

type alias Dimensions = (List Float, List Float)


type alias Model a = (Index, (Dimensions, List a))


type Msg 
  = RotateRight
  | RotateLeft
  | Content
  | GotElement Index (Result Browser.Dom.Error Browser.Dom.Element)


getWidth : Int -> Cmd Msg
getWidth index =
  Browser.Dom.getElement (itemId index) |> Task.attempt (GotElement index)


newModel : Model (Html msg)
newModel = 
 let 
  zeros = (List.map (\_ ->  0.0) someItems)
 in 
 (0, ((zeros, zeros), someItems))


init : Maybe Int -> (Model (Html msg), Cmd Msg)
init x =
  let  
    ids = Debug.log "ids" (List.range 0 <| List.length someItems)
    cmds = List.map getWidth (List.range 0 <| List.length someItems)
  in 
  (newModel, Cmd.batch <| cmds)


someItems : List (Html msg)
someItems = 
  [ Components.box <| [Components.label "A"]
  , Components.box <| [Components.label "B"]
  , Components.box <| [Components.label "C"]
  , Components.box <| [Components.label "D"]
  , Components.box <| [Components.label "E"]
  ]


apply : Msg -> Model (Html msg) -> Model (Html msg)
apply msg ((index, ((widths, heights), items) as data) as model) = 
  case msg of
    RotateRight -> (index - 1, data)
    RotateLeft -> (index + 1, data)
    _ -> (index, data)



update : Msg -> Model (Html msg) -> (Model (Html msg), Cmd msg)
update msg ((index, ((widths, heights), items)) as model) = 
  case msg of 
    GotElement _ (Err err) -> (model, Cmd.none)
    GotElement i (Ok element) -> 
     let
       widths_ = Tools.replaceAt i element.element.width widths
       heights_ = Tools.replaceAt i element.element.height heights
     in 
     ((index, ((widths_, heights_), items)), Cmd.none)

    _ -> (apply msg model, Cmd.none)


itemId : Int -> String
itemId index =
  "carousel-item-" ++ String.fromInt index


isFocused : Int -> Int -> Int -> Bool
isFocused count index offset =
  let
    curr = abs (modBy count index)
  in 
  curr == offset


maxWidth : List Float -> Float
maxWidth widths =
  List.foldl (\prev next ->
    if prev > next then prev else next) 0 widths


modPercent : Int -> Int -> Float
modPercent count index =
  1.0 - (toFloat (modBy count index)/ toFloat count)


item : Int -> Int -> Int -> Float -> Html msg -> Html Msg
item count index offset width thing =
  let
    hasFocus = isFocused count index offset
    deg = (toFloat (index + offset)) * (360.0 / (toFloat count))
    rotate = "rotateY(" ++ String.fromFloat deg  ++ "deg) translateZ(" ++ String.fromFloat (3 * width) ++ "px)"
    zIndex = if hasFocus then Attr.style "z-index" "2" else Attr.style "" ""
    opacity = Attr.style "opacity" <| if hasFocus then "1" else String.fromFloat <| modPercent count (offset + index)
  in 
  Html.map (\_ -> Content) <| 
    div [zIndex, opacity, Attr.id <| itemId offset, Attr.class "item", Attr.style "transform" rotate] [ thing ]



view : (Model (Html msg)) -> Msg -> Msg -> Html Msg
view (index, ((widths, heights), items)) left right = 
  let
    height = maxWidth heights
  in 
  Components.box <|
  [ div [Attr.class "carousel is-flex is-align-items-center", Attr.style "min-height" <| String.fromFloat (2 * height) ++ "px"] <| 
      List.map3 (\i width x -> item (List.length items) index i (maxWidth widths) x) 
        (List.range 0 (List.length items)) widths items 
  , div [Attr.class "is-relative is-flex is-justify-content-space-around"
        , Attr.style "margin-top" <| "-" ++ String.fromFloat (height / 2) ++ "px" ]
    [ Components.svgClick "arrow-left" left
    , Components.svgClick "arrow-right" right
    ]
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
