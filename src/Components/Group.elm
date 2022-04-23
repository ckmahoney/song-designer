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


smallBox : Html msg -> Html msg
smallBox el = 
  div [Attr.class "box m-6 is-vcentered"] [ el ]


overview : (Msg a -> msg) -> (Int -> a -> Html msg) -> List a -> Html msg
overview revise  thumb things =
  let
    withActions = (\i el -> 
      div [Attr.class "box m-6"] <| List.singleton <| Components.col [] 
        [ thumb i el
        , Components.colsMulti  <| 
          List.map Components.colHalf 
            [ if i > 0 then 
                Components.button (revise <| MoveTo el i (i - 1)) [Attr.class "is-fullwidth"] "Move Left" 
                else text ""
            , if i < -1 + List.length things then 
              Components.button (revise <| MoveTo el i (i + 1)) [Attr.class "is-fullwidth"] "Move Right" 
                else text ""
            , Components.button (revise <| InsertAt (i + 1) el) [Attr.class "is-fullwidth"] "Duplicate"
            , Components.button (revise <| Delete i) [Attr.class "is-fullwidth"] ("Delete Arc " ++ String.fromInt (i + 1)) 
            ]
        ] )

  in 
  Components.colsWith [Attr.class "is-vcentered"] <| 
    (List.indexedMap withActions things)


inserter : (Msg a -> msg) -> a -> (Int -> a -> Html msg) -> List a -> Html msg
inserter revise newEl thumb things =
  let
    adder = (\i -> div [Attr.class "is-flex is-flex-direction-column is-justify-content-center my-6 mx-1"] [Components.button (revise <| InsertAt i newEl) [] "+"])
    withActions = (\i el -> 
      div [Attr.class "is-flex is-flex-direction-row"] -- keep these elements horizontal 
        [ adder i
        , div [Attr.class "box my-6 mx-3 column"] 
          [ thumb i el
          , Components.colsMulti  <| 
            List.map Components.colHalf 
              [ if i > 0 then 
                  Components.button (revise <| MoveTo el i (i - 1)) [Attr.class "is-fullwidth"] "Move Left" 
                  else text ""
              , if i < -1 + List.length things then 
                Components.button (revise <| MoveTo el i (i + 1)) [Attr.class "is-fullwidth"] "Move Right" 
                  else text ""
              , Components.button (revise <| InsertAt (i + 1) el) [Attr.class "is-fullwidth"] "Duplicate"
              , Components.button (revise <| Delete i) [Attr.class "is-fullwidth"] ("Delete Arc " ++ String.fromInt (i + 1)) 

              ]
          ]
        , if i == List.length things - 1 then 
          (adder <| i + 1) 
            else text ""  ] )
  in 
  Components.colsWith [Attr.class "is-vcentered"] <| 
    (List.indexedMap withActions things)

view : List a -> (Int -> a -> Html msg) -> (Msg a -> msg) -> msg -> Html msg
view children thumb revise create = 
  overview revise thumb children

main = text ""


-- main = Browser.element 
--   { init = init
--   , update = update
--   , view = view
--   , subscriptions = (\_ -> Sub.none)
--   }
