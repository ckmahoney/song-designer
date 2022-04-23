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
import ScoreMeta as ScoreMeta
import Chords
import Json.Decode as Decode


type alias CardGroup = Group.Model Card.Model

-- the word "Save" is used to describe the act of closing 
-- an editor with the new value applied to parent state

type Msg
  = ViewCard Card.Model
  | EditCard Card.Model
  | SaveCard Card.Model
  | CloseCard
  | UpdateCard Card.Msg

  | CreateCard
  | EditGroup (Group.Msg Card.Model)

  | EditMeta
  | SaveMeta ScoreMeta.Model
  | CloseMeta
  | UpdateMeta ScoreMeta.Msg


type State 
  = Viewing ScoreMeta.Model (Group.Model Card.Model)
  | EditingCards ScoreMeta.Model (Group.Model Card.Model) Card.State
  | EditingMeta ScoreMeta.Model (Group.Model Card.Model) ScoreMeta.State 


someCards : List Card.Model
someCards = 
  [ Card.new
  , Card.new2
  , Card.new3
  ]


new : State
new = 
  Viewing ScoreMeta.empty <| Group.from someCards


init : Maybe Int -> (State, Cmd msg)
init flags = 
  case flags of 
    _ -> (new, Cmd.none)


update : Msg -> State -> (State, Cmd msg)
update msg state = 
  case state of 
    Viewing meta group -> 
      case msg of  
        CreateCard -> 
          (Viewing meta (Tuple.first group, List.append (Tuple.second group) [Card.create]), Cmd.none)

        ViewCard card -> 
          let
             index = Tools.findIndex card (Tuple.second group)
             newGroup = Group.by index (Tuple.second group)
          in 
          (EditingCards meta newGroup <| Card.editCard card, Cmd.none)

        EditGroup gMsg -> 
          let
            group2 = Group.apply gMsg group
          in
          (Viewing meta group2, Cmd.none)

        EditMeta ->
          (EditingMeta meta group <| ScoreMeta.Editing meta meta, Cmd.none)

        _ -> (state, Cmd.none)

    EditingCards meta ((index, cards) as group) cardState -> 
      case msg of 
        UpdateCard cMsg ->     
          (EditingCards meta group (Card.apply cMsg cardState), Cmd.none)

        SaveCard next -> 
          let
            i = Maybe.withDefault -1 index
            newGroup = (Nothing, Tools.replaceAt i next cards)
          in
          (Viewing meta newGroup, Cmd.none)

        CloseCard -> 
          (Viewing meta group, Cmd.none)

        EditCard card -> 
          let
            i = Tools.findIndex card cards
            newGroup = (Just i, cards)
          in
          (EditingCards meta newGroup <| Card.Editing card card, Cmd.none)

        _ -> (state, Cmd.none)

    EditingMeta orig group metaState ->
      case msg of
        UpdateMeta mMsg ->
          (EditingMeta orig group (ScoreMeta.apply mMsg metaState), Cmd.none)

        SaveMeta meta ->
          (Viewing meta group, Cmd.none)
          
        CloseMeta ->
          (Viewing orig group, Cmd.none)

        _ ->
          (state, Cmd.none)


view : State -> msg -> (ScoreMeta.Msg -> msg) -> (ScoreMeta.Model -> msg) ->  msg -> (Card.Model -> msg) -> (Card.Model -> msg) -> (Card.Msg -> msg) -> (Card.Model -> msg) -> msg -> msg -> (Group.Msg Card.Model -> msg) -> Html msg
view state editMeta changeMeta saveMeta closeMeta openCard editCard change save cancel createCard editGroup =
  case state of
    Viewing meta (mIndex, cards) ->
      let
        nCycles = List.foldl (\card sum -> sum + (2 ^ card.size) ) 0 cards 
      in
      Components.box 
        [ ScoreMeta.readonly nCycles meta editMeta
        , Group.inserter editGroup Card.empty (\i c -> Card.stub c (openCard c))  cards
        ]

    EditingCards meta group cardState -> 
      case cardState of 
        Card.Viewing card -> Card.readonly card (editCard card) cancel
        Card.Editing orig next -> Card.editor next change (save next) cancel

    EditingMeta orig group metaState -> 
      case metaState of 
        ScoreMeta.Editing prev next ->
          ScoreMeta.editor next changeMeta (saveMeta next) closeMeta
        _ ->
          text "How did you get here"


main = 
  Browser.element 
    { init = init
    , update = update
    , view = (\state -> view state EditMeta UpdateMeta SaveMeta CloseMeta ViewCard EditCard UpdateCard SaveCard CloseCard CreateCard EditGroup)
    , subscriptions = (\_ -> Sub.none)
    }
