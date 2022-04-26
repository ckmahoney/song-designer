module Designer.Chart exposing (..)

import Browser
import Html exposing (Html, h1, button, div, text, label, p, input,b)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onInput)

import Configs
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
import Json.Encode as Encode
import Http

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

  | ReqTrack ScoreMeta.Model (List Card.Model)
  | GotTrack (Result Http.Error ())

type State 
  = Viewing ScoreMeta.Model (Group.Model Card.Model)
  | EditingCard ScoreMeta.Model (Group.Model Card.Model) Card.State
  | EditingMeta ScoreMeta.Model (Group.Model Card.Model) ScoreMeta.State 


encodeScoreMeta : ScoreMeta.Model -> Encode.Value
encodeScoreMeta {title, bpm, key, cpc} =
  Encode.object
    [ ("title", Encode.string title)
    , ("bpm", Encode.float bpm)
    , ("key", Encode.int key)
    , ("cpc", Encode.int cpc)
    ]


encodeArc : Card.Model -> Encode.Value
encodeArc {title, size, style}  =
  Encode.object
    [ ("title", Encode.string title)
    , ("size", Encode.int size )
    , ("style", Encode.string <| String.toLower <| Card.styleLabel style)
    ]



encodeReqTrackNext : ScoreMeta.Model -> List  Card.Model -> Encode.Value
encodeReqTrackNext meta arcs =
  Encode.object
    [ ("meta", encodeScoreMeta meta)
    , ("arcs", Encode.list encodeArc arcs)
    ]


reqTrack : ScoreMeta.Model -> List Card.Model -> (Result Http.Error () -> msg) -> Cmd msg
reqTrack meta arcs complete =
  Http.post
    { url = Configs.apiUrl "track/next"
    , body = Http.jsonBody <| encodeReqTrackNext meta arcs
    , expect = Http.expectWhatever complete
    }


someCards : List Card.Model
someCards = 
  [ Card.new
  , Card.new2
  , Card.new3
  ]


new : State
new = 
  Viewing ScoreMeta.empty <| Group.from someCards
  -- EditingCard ScoreMeta.empty (Group.from someCards) (Card.Editing Card.new Card.new)


init : Maybe Int -> (State, Cmd msg)
init flags = 
  (new, Cmd.none)


update : Msg -> State -> (Result Http.Error () -> msg) -> (State, Cmd msg)
update msg state complete = 
  case msg of  -- "preflight" check
    ReqTrack meta arcs ->
      (state, reqTrack meta arcs complete)

    GotTrack _ ->
      (state, Cmd.none)

    _ -> 
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
            (EditingCard meta newGroup <| Card.editCard card, Cmd.none)

          EditGroup gMsg -> 
            let
              group2 = Group.apply gMsg group
            in
            (Viewing meta group2, Cmd.none)

          EditMeta ->
            (EditingMeta meta group <| ScoreMeta.Editing meta meta, Cmd.none)

          _ -> (state, Cmd.none)

      EditingCard meta ((index, cards) as group) cardState -> 
        case msg of 
          UpdateCard cMsg ->     
            (EditingCard meta group (Card.apply cMsg cardState), Cmd.none)

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
            (EditingCard meta newGroup <| Card.Editing card card, Cmd.none)

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

    EditingCard meta group cardState -> 
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
    , update = (\msg model -> update msg model GotTrack)
    , view = (\state -> view state EditMeta UpdateMeta SaveMeta CloseMeta ViewCard EditCard UpdateCard SaveCard CloseCard CreateCard EditGroup)
    , subscriptions = (\_ -> Sub.none)
    }
