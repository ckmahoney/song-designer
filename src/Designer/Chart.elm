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
  = ViewCard Card.Model
  | EditCard Card.Model
  | SaveCard Card.Model
  | CloseCard
  | UpdateCard Card.Msg


type State 
  = Viewing (Group.Model Card.Model)
  | Editing (Group.Model Card.Model) Card.State
 
someCards : List Card.Model
someCards = 
  [ Card.new
  , Card.new2
  , Card.new3
  ]


new : State
new = 
  Viewing <| Group.from someCards


init : Maybe Int -> (State, Cmd msg)
init flags = 
  case flags of 
    _ -> (new, Cmd.none)


update : Msg -> State -> (State, Cmd msg)
update msg state = 
  case state of 
    Viewing group -> 
      case msg of  
        ViewCard card -> 
          let
             index = Debug.log "Editing card at index" <| Tools.findIndex card (Tuple.second group)
             newGroup = Group.by index (Tuple.second group)
          in 
          (Editing newGroup <| Card.editCard card, Cmd.none)
        _ -> (state, Cmd.none)

    Editing ((index, cards) as group) cardState -> 
      case msg of 
        UpdateCard cMsg ->     
          (Editing group (Card.apply cMsg cardState), Cmd.none)

        SaveCard next -> 
          let
            i = Maybe.withDefault -1 index
            newGroup = (Just i, Tools.replaceAt i next cards)
          in
          (Viewing newGroup, Cmd.none)

        CloseCard -> 
          (Viewing group, Cmd.none)

        EditCard card -> 
          let
            i = Debug.log "Editing the card at " Tools.findIndex card cards
            newGroup = (Just i, cards)
          in
          (Editing newGroup <| Card.Editing card card, Cmd.none)

        _ -> (state, Cmd.none)


viewCard : Card.State -> msg -> msg -> msg -> Html msg
viewCard state start save cancel  =
  Card.viewSlim state start save cancel


view : State -> (Card.Model -> msg) -> (Card.Model -> msg) -> (Card.Msg -> msg) -> (Card.Model -> msg) -> msg -> Html msg
view state open edit change save cancel = 
  case state of
    Viewing group ->
      Group.view group (\c -> Card.stub c (open c))

    Editing group cardState -> 
      case cardState of 
        Card.Viewing card -> Card.readonly card (edit card) cancel
        Card.Editing orig next -> Card.editor next change (save next) cancel


main = 
  Browser.element 
    { init = init
    , update = update
    , view = (\state -> view state ViewCard EditCard UpdateCard SaveCard CloseCard)
    , subscriptions = (\_ -> Sub.none)
    }
