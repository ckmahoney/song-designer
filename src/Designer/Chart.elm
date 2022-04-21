module Designer.Chart exposing (..)

import Browser
import Html exposing (Html, h1, button, div, text, label, p, input,b)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onInput)

import View 
import Elements
import Tools
import Components
import Data
import Components.Group as Group
import Card as Card

import Json.Decode as Decode


type alias CardGroup = Group.Model Card.Model


type Msg
  = OpenCard Card.Model
  | SaveCard 
  | KillCard


type alias State = (Group.Model Card.Model, Maybe Card.State)


someCards : List Card.Model
someCards = 
  [ Card.new
  , Card.new2
  , Card.new3
  ]


new : State
new = 
  (Group.from someCards, Nothing)


init : Maybe Int -> (State, Cmd msg)
init flags = 
  case flags of 
    _ -> (new, Cmd.none)


update : Msg -> State -> (State, Cmd msg)
update msg state = 
  case state of 
    (group, Nothing) ->    
      case msg of  
        OpenCard card -> 
          let
             index = Tools.findIndex card (Tuple.second group)
             next = Group.by index (Tuple.second group)
          in 
          ((group, Just <| Card.editCard card), Cmd.none)
        _ -> (state, Cmd.none)

    ((mIndex, cards), Just card) -> (state, Cmd.none)


viewCard : Card.State -> msg -> msg -> msg -> Html msg
viewCard state start save cancel  =
  Card.viewSlim state start save cancel


view : State -> (Card.Model -> msg) -> msg -> msg -> Html msg
view state open save cancel = 
  case state of 
    (group, Nothing) ->
      Group.view group (\c -> Card.stub c (open c))

    (group, Just editing) -> 
      case editing of 
        Card.Viewing card -> Card.viewSlim editing (open card) save cancel
        Card.Editing orig next -> Card.viewSlim editing (open next) save cancel
        
      


main = 
  Browser.element 
    { init = init
    , update = update
    , view = (\state -> view state OpenCard SaveCard KillCard)
    , subscriptions = (\_ -> Sub.none)
    }
