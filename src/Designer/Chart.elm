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
import Decoders
import Http
import Types exposing (WithMember, GhostMember, TrackMeta)
import Playback2 as Player

type alias CardGroup = Group.Model Card.Model

-- the word "Save" is used to describe the act of closing 
-- an editor with the new value applied to parent state

type ChartMsg
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

  | UpdatePlayer Player.Msg
  | Download String

  | ReqTrack ScoreMeta.Model (List Card.Model)
  | GotTrack (Result Http.Error TrackMeta)

type State 
  = Viewing Player.Model ScoreMeta.Model (Group.Model Card.Model)
  | EditingCard Player.Model ScoreMeta.Model (Group.Model Card.Model) Card.State
  | EditingMeta Player.Model ScoreMeta.Model (Group.Model Card.Model) ScoreMeta.State 




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


reqTrack : ScoreMeta.Model -> List Card.Model -> (Result Http.Error TrackMeta -> msg) -> Cmd msg
reqTrack meta arcs complete =
  Http.post
    { url = Configs.apiUrl "track/next"
    , body = Http.jsonBody <| encodeReqTrackNext meta arcs
    , expect = Http.expectJson complete Decoders.decodeTrack
    }


someCards : List Card.Model
someCards = 
  [ Card.new
  , Card.new2
  , Card.new3
  ]


newState : State
newState = 
  Viewing Player.new ScoreMeta.empty <| Group.from someCards



init : Maybe GhostMember -> (WithMember State, Cmd msg)
init flags = 
  case flags of 
    Nothing -> 
      ((Configs.anonMember, newState), Cmd.none)
    
    Just member ->
      ((member, newState), Cmd.none)



apply : ChartMsg -> WithMember State -> State
apply msg (member, state) = 
   case state of 
      Viewing player meta group -> 
        case msg of  
          GotTrack result ->
            case result of 
              Ok track  -> 
                let
                  prefix = if Configs.devMode == True then "http://localhost:3000" else ""
                  p =  Player.add player { track  | filepath = prefix ++ track.filepath } 
                in 
                Viewing p meta group

              Err error -> 
                state

          CreateCard -> 
            Viewing player meta (Tuple.first group, List.append (Tuple.second group) [Card.create])

          ViewCard card -> 
            let
               index = Tools.findIndex card (Tuple.second group)
               newGroup = Group.by index (Tuple.second group)
            in 
            EditingCard player meta newGroup <| Card.editCard card

          EditGroup gMsg -> 
            let
              group2 = Group.apply gMsg group
            in
            Viewing player meta group2

          EditMeta ->
            EditingMeta player meta group <| ScoreMeta.Editing meta meta

          _ -> state

      EditingCard player meta ((index, cards) as group) cardState -> 
        case msg of 
          UpdateCard cMsg ->     
            EditingCard player meta group (Card.apply cMsg cardState)

          SaveCard next -> 
            let
              i = Maybe.withDefault -1 index
              newGroup = (Nothing, Tools.replaceAt i next cards)
            in
            Viewing player meta newGroup

          CloseCard -> 
            Viewing player meta group

          EditCard card -> 
            let
              i = Tools.findIndex card cards
              newGroup = (Just i, cards)
            in
            EditingCard player meta newGroup <| Card.Editing card card

          GotTrack result ->
            case result of 
              Ok track  -> 
                let
                  p =  Player.add player track 
                in 
                EditingCard p meta group cardState

              Err error -> 
                state


          _ -> state


      EditingMeta player orig group metaState ->
        case msg of
          UpdateMeta mMsg ->
            EditingMeta player orig group (ScoreMeta.apply mMsg metaState)

          SaveMeta meta ->
            Viewing player meta group

          CloseMeta ->
            Viewing player orig group

          GotTrack result -> 
            case result of 
              Ok track  -> 
                let
                  p = Player.add player track 
                in 
                EditingMeta p orig group metaState

              Err error -> 
                state

          _ ->
            state


update : ChartMsg -> WithMember State -> (Result Http.Error TrackMeta -> msg) -> (WithMember State, Cmd msg)
update msg ((member, state) as model) onComplete =
  case msg of 
    ReqTrack meta arcs ->
      ((member, state), reqTrack meta arcs onComplete)

    Download path -> 
      ((member, state), Configs.download <| Debug.log "triggerd a download" path)

    UpdatePlayer pMsg -> 
      let
        next = (\p -> Tuple.first <| Player.update pMsg p) 
        cmdr = (\p -> Player.trigger <| Tuple.second <| Player.update pMsg p) 
      in
      case state of 
        Viewing player meta group ->
         ((member, Viewing (next player) meta group), cmdr player)

        EditingCard player meta ((index, cards) as group) cardState ->      
         ((member, EditingCard (next player) meta group cardState), cmdr player)
          
        EditingMeta player orig group metaState ->
         ((member, EditingMeta (next player) orig group metaState), cmdr player)          

    _ -> 
      ((member, apply msg model), Cmd.none) 


view : WithMember State -> 
  (Player.Msg -> msg) ->
  (String -> msg) -> 
  msg -> (ScoreMeta.Msg -> msg) -> (ScoreMeta.Model -> msg) ->  msg -> (Card.Model -> msg) -> (Card.Model -> msg) -> (Card.Msg -> msg) -> (Card.Model -> msg) -> msg -> msg -> (Group.Msg Card.Model -> msg) 
  -> (ScoreMeta.Model -> (List Card.Model) -> msg)
  -> Html msg
view (member, state) updatePlayer download editMeta changeMeta saveMeta closeMeta openCard editCard change save cancel createCard editGroup doRequest =
  div [] 
  [ div [Attr.id "the-player"] []
  , case state of
      Viewing player meta (mIndex, cards) ->
        let
          nCycles = List.foldl (\card sum -> sum + (2 ^ card.size) ) 0 cards 
        in
       
        Components.box 
          [ Player.view player updatePlayer download
          , ScoreMeta.readonly nCycles meta editMeta
          , Group.inserter editGroup Card.empty (\i c -> Card.stub c (openCard c))  cards
          , Components.button (doRequest meta cards) [] "Make a Song"
          ]

      EditingCard player meta group cardState -> 
        case cardState of 
          Card.Viewing card -> Card.readonly card (editCard card) cancel
          Card.Editing orig next -> Card.editor next change (save next) cancel

      EditingMeta player orig group metaState -> 
        case metaState of 
          ScoreMeta.Editing prev next ->
            ScoreMeta.editor next changeMeta (saveMeta next) closeMeta
          _ ->
            text "How did you get here"
    ] 


main = 
  Browser.element 
    { init = init
    , update = (\msg model -> update msg model GotTrack)
    , view = (\(member, state) -> view (member, state) UpdatePlayer Download EditMeta UpdateMeta SaveMeta CloseMeta ViewCard EditCard UpdateCard SaveCard CloseCard CreateCard EditGroup ReqTrack)
    , subscriptions = (\_ -> Sub.none)
    }
