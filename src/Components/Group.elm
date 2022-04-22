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
  | MoveTo a Index Index
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
    View -> 
      (Nothing, children)

    InsertAt index el ->
      (Nothing, Tools.insertAt index el children)

    MoveTo target fromI toI ->
      let
        filtered = Tools.removeAt fromI children
      in 
      (Nothing, Tools.insertAt toI target filtered)

    Delete index -> 
      (Nothing, Tools.removeAt index children)


update : (Msg a) -> Model a -> (Model a, Cmd msg)
update msg state = 
  case msg of 
    _ -> (state, Cmd.none)


overview : (Msg a -> msg) -> msg -> (Int -> a -> Html msg) -> List a -> Html msg
overview revise create thumb things =
  let
    withActions = (\i el -> 
      div []
        [ thumb i el
        , Components.button (revise <| Delete i) [] ("Delete Arc " ++ String.fromInt (i + 1)) 
        , Components.button (revise <| InsertAt (i + 1) el) [] "Duplicate"
        , if i > 0 then 
            Components.button (revise <| MoveTo el i (i - 1)) [] "Move Left" 
            else text ""
        , if i < -1 + List.length things then 
            Components.button (revise <| MoveTo el i (i + 1)) [] "Move Right" 
            else text ""
        ] )


  in 
  div [] <| 
    (List.indexedMap withActions things)
    ++ [(Components.button create [] "Add Another")]


view : List a -> (Int -> a -> Html msg) -> (Msg a -> msg) -> msg -> Html msg
view children thumb revise create = 
  overview revise create thumb children

main = text ""


-- main = Browser.element 
--   { init = init
--   , update = update
--   , view = view
--   , subscriptions = (\_ -> Sub.none)
--   }
