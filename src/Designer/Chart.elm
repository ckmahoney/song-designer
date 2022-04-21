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

  | CreateCard
  | EditGroup (Group.Msg Card.Model)

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
        CreateCard -> 
          (Viewing (Tuple.first group, List.append (Tuple.second group) [Card.create]), Cmd.none)

        ViewCard card -> 
          let
             index = Debug.log "Editing card at index" <| Tools.findIndex card (Tuple.second group)
             newGroup = Group.by index (Tuple.second group)
          in 
          (Editing newGroup <| Card.editCard card, Cmd.none)

        EditGroup gMsg -> 
          let
            group2 = Group.apply gMsg group
          in
          (Viewing group2, Cmd.none)
        _ -> (state, Cmd.none)

    Editing ((index, cards) as group) cardState -> 
      case msg of 
        UpdateCard cMsg ->     
          (Editing group (Card.apply cMsg cardState), Cmd.none)

        SaveCard next -> 
          let
            i = Maybe.withDefault -1 index
            newGroup = (Nothing, Tools.replaceAt i next cards)
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



view : State -> (Card.Model -> msg) -> (Card.Model -> msg) -> (Card.Msg -> msg) -> (Card.Model -> msg) -> msg -> msg -> (Group.Msg Card.Model -> msg) -> Html msg
view state open edit change save cancel createCard editGroup = 
  case state of
    Viewing (mIndex, children) ->
      Group.view children (\i c -> Card.stub c (open c)) editGroup createCard

    Editing group cardState -> 
      case cardState of 
        Card.Viewing card -> Card.readonly card (edit card) cancel
        Card.Editing orig next -> Card.editor next change (save next) cancel


main = 
  Browser.element 
    { init = init
    , update = update
    , view = (\state -> view state ViewCard EditCard UpdateCard SaveCard CloseCard CreateCard EditGroup)
    , subscriptions = (\_ -> Sub.none)
    }
