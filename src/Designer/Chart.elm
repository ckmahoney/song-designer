module Designer.Chart exposing (..)

import Browser
import Html exposing (Html, h1, h2, h3, button, div, text, label, p, input,b, details, summary, br)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onInput)

import Configs
import View 
import Elements
import Tools
import Components
import Data
import Components.Group as Group
import Arc as Arc
import ScoreMeta as ScoreMeta
import Chords
import Json.Decode as Decode
import Json.Encode as Encode
import Encoders
import Comm.Decoders 
import Comm.Post
import Http
import Types exposing (WithMember, TrackResponse, GhostMember, TrackMeta)
import Playback2 as Player

type alias ArcGroup = Group.Model Arc.Model

-- the word "Save" is used to describe the act of closing 
-- an editor with the new value applied to parent state

type ChartMsg
  = ViewArc Arc.Model
  | EditArc Arc.Model
  | SaveArc Arc.Model
  | CloseArc
  | UpdateArc Arc.Msg

  | CreateArc
  | EditGroup (Group.Msg Arc.Model)

  | EditMeta
  | SaveMeta ScoreMeta.Model
  | CloseMeta
  | UpdateMeta ScoreMeta.Msg

  | UpdatePlayer Player.Msg
  | Download String

  | ReqTrack ScoreMeta.Model (List Arc.Model)
  | GotTrack (Result Http.Error TrackResponse)
  | GotTracks (Result Http.Error (List TrackMeta))


type alias Model = 
  { meta : ScoreMeta.Model
  , arcs : Group.Model Arc.Model
  , player : Player.Model
  , httpMessage : String 
  }


type State 
  = Viewing Model 
  | EditingArc Model Arc.State
  | EditingMeta Model ScoreMeta.State 
  | Requesting Model 



encodeScoreMeta : ScoreMeta.Model -> Encode.Value
encodeScoreMeta {title, bpm, key, cpc} =
  Encode.object
    [ ("title", Encode.string title)
    , ("bpm", Encode.float bpm)
    , ("key", Encode.int key)
    , ("cpc", Encode.int cpc)
    ]


encodeArc : Arc.Model -> Encode.Value
encodeArc {title, size, style}  =
  Encode.object
    [ ("title", Encode.string title)
    , ("size", Encode.int size )
    , ("style", Encode.string <| String.toLower <| Arc.styleLabel style)
    ]



encodeReqTrack : ScoreMeta.Model -> List  Arc.Model -> Encode.Value
encodeReqTrack meta arcs =
  Encode.object
    [ ("meta", encodeScoreMeta meta)
    , ("arcs", Encode.list encodeArc arcs)
    ]


encodeSongRequest : GhostMember -> ScoreMeta.Model -> List Arc.Model -> Encode.Value
encodeSongRequest member meta arcs =
  Encode.object
    [ ("member", Encoders.encodeMember member)
    , ("scoredef", encodeReqTrack meta arcs)
    ]


reqTrack : GhostMember -> ScoreMeta.Model -> List Arc.Model -> (Result Http.Error TrackResponse -> msg) -> Cmd msg
reqTrack member meta arcs complete =
  Http.post
    { url = Configs.apiUrl "track/next"
    , body = Http.jsonBody <| encodeSongRequest member meta arcs
    , expect = Http.expectJson complete Comm.Decoders.decodeTrackResponse
    }


someArcs : List Arc.Model
someArcs = 
  [ Arc.new
  , Arc.new2
  , Arc.new3
  , Arc.new2
  , Arc.new3
  , Arc.new4
  , Arc.new3
  , Arc.new3
  , Arc.new
  ]


newState : State
newState = 
  Viewing 
    { player = Player.new
    , meta = ScoreMeta.empty
    , arcs = Group.from someArcs
    , httpMessage = ""
    }


isAnon : GhostMember -> Bool
isAnon member =
  member == Configs.anonMember


init : Maybe GhostMember -> (Result Http.Error (List TrackMeta) -> msg)  -> (WithMember State, Cmd msg)
init flags loadTracks = 
  case flags of 
    Nothing -> 
      ((Configs.anonMember, newState), Cmd.none)
    
    Just member ->
      ((member, newState), Comm.Post.fetchSongs member.email member.uuid loadTracks)


apply : ChartMsg -> WithMember State -> State
apply msg (member, state) = 
   case state of 
      Viewing ({player, meta, arcs} as model) -> 
        case msg of  
          CreateArc -> 
            Viewing { model | arcs = (Tuple.first arcs, List.append (Tuple.second arcs) [Arc.create]) }

          ViewArc card -> 
            let
               index = Tools.findIndex card (Tuple.second arcs)
               newArcs = Group.by index (Tuple.second arcs)
            in 
            EditingArc { model | arcs =  newArcs } <| Arc.editArc card

          EditGroup gMsg -> 
            let
              arcs2 = Group.apply gMsg arcs
            in
            Viewing { model | arcs = arcs2 }

          EditMeta ->
            EditingMeta model <| ScoreMeta.Editing meta meta

          _ -> 
            state

      EditingArc ({player,  meta, arcs} as model) cardState -> 
        case msg of 
          UpdateArc cMsg ->     
            EditingArc model (Arc.apply cMsg cardState)

          SaveArc next -> 
            let
              (index, cards) = arcs
              i = Maybe.withDefault -1 index
              newArcs = (Nothing, Tools.replaceAt i next cards)
            in
            Viewing { model | arcs = newArcs }

          CloseArc -> 
            Viewing model

          EditArc card -> 
            let
              cards = Tuple.second arcs
              i = Tools.findIndex card cards
              newArcs = (Just i, cards)
            in
            EditingArc { model | arcs =  newArcs } <| Arc.Editing card card

          GotTrack result ->
            case result of 
              Ok {message, track}  -> 
                let
                  
                  p =  Player.add player track
                in 
                EditingArc { model | player = p } cardState

              Err error -> 
                state

          _ -> state


      EditingMeta ({player, arcs} as model) metaState ->
        case msg of
          UpdateMeta mMsg ->
            EditingMeta model (ScoreMeta.apply mMsg metaState)

          SaveMeta meta ->
            Viewing { model | meta = meta }

          CloseMeta ->
            Viewing model 

          GotTrack result -> 
            case result of 
              Ok {message, track}  -> 
                let
                  p = Player.add player track 
                in 
                EditingMeta { model | player = p } metaState

              Err error -> 
                state

          _ ->
            state

      _ ->
        state


update : ChartMsg -> WithMember State -> (Result Http.Error TrackResponse -> msg) -> (WithMember State, Cmd msg)
update msg (member, state) onComplete =
  case msg of 
    GotTracks res ->
      case res of 
        Ok tracks -> 
         case state of 
           Requesting ({player, meta, arcs} as model) ->
              let
                prefix = if Configs.devMode == True then "http://localhost:3000" else ""
                p =  Player.addMany player tracks
              in 
              ((member, Requesting { model | player = p }), Cmd.none)


           Viewing  ({player, meta, arcs} as model) ->
              let
                prefix = if Configs.devMode == True then "http://localhost:3000" else ""
                p =  Player.addMany player tracks
              in 
              ((member, Viewing  { model | player = p }), Cmd.none)


           EditingArc  ({player, meta, arcs} as model) arcState->
              let
                prefix = if Configs.devMode == True then "http://localhost:3000" else ""
                p =  Player.addMany player tracks
              in 
              ((member, EditingArc { model | player = p } arcState), Cmd.none)


           EditingMeta  ({player, meta, arcs} as model) metaState ->
              let
                prefix = if Configs.devMode == True then "http://localhost:3000" else ""
                p =  Player.addMany player tracks
              in 
              ((member, EditingMeta { model | player = p } metaState), Cmd.none)

        Err error -> 
          ((member, state), Cmd.none)
   
    GotTrack res ->
      case res of 
        Ok {message, track} -> 
         case state of 
           Requesting ({player, meta, arcs} as model) ->
              let
                prefix = if Configs.devMode == True then "http://localhost:3000" else ""
                p =  Player.add player { track  | filepath = prefix ++ track.filepath } 
              in 
              ((member, Viewing { model | player = p }), Cmd.none)

           _ ->
            ((member, state), Cmd.none)

        Err error -> 
          ((member, state), Cmd.none)

    ReqTrack meta arcs ->
      case state of 
        Viewing model ->
         ((member, Requesting model), reqTrack member meta arcs onComplete)

        _ ->   
         ((member, state), Cmd.none)

    Download path -> 
      ((member, state), Configs.download path)

    UpdatePlayer pMsg -> 
      let
        next = (\p -> Tuple.first <| Player.update pMsg p) 
        cmdr = (\p -> Player.trigger <| Tuple.second <| Player.update pMsg p) 
      in
      case state of 
        Requesting  ({player, meta, arcs} as model) ->
         ((member, Requesting { model | player = (next player)}), cmdr player)

        Viewing  ({player, meta, arcs} as model) ->
         ((member, Viewing { model | player = (next player) }), cmdr player)

        EditingArc ({player, meta,  arcs} as model) cardState ->      
         ((member, EditingArc { model | player = (next player)} cardState), cmdr player)
          
        EditingMeta ({player} as model) metaState ->
         ((member, EditingMeta { model | player = (next player) } metaState), cmdr player)          

    _ -> 
      ((member, apply msg (member, state)), Cmd.none)



allTheDetails : List (Html msg)
allTheDetails =
  [ details [] 
          [ summary [Attr.style "cursor" "pointer", Attr.class "is-size-4"] [ text "What are Song Details?"  ]
          , div [Attr.class "m-3" ]
              [ p [] [ text "No matter what, ALL music contains these two things in common: It moves through time, and occupies space! " ]
            , p [] [ text "To measure time we use Beats Per Minute. A higher number means the song will sound faster, while lower numbers sound slower." ]
            , p [] [ text "The space a song fills is determined by its key signature. That's a term musicians use to describe which notes to choose from. We just call it \"color.\" Let's use actual colors for a comparison." ]
            , p [] [ text "Yellow, purple, blue, orange, red, and green are great colors and each one is about equal to the others." ]
            , p [] [ text "When listening to music, the same idea is true for all 12 keys." ]
            , p [] [ text "Making songs in a different keys is the fastest way to get a wide variety of results." ]
              ] ]
      , details [] 
          [ summary [Attr.style "cursor" "pointer", Attr.class "is-size-4"] [ text "What is a Layout?" ]
          , div [Attr.class "m-3"]
            [ p [] [ text "Most music has a few distinct sections that get used over and over and over again. We call each of these sections an \"Arc.\" The sequence of Arcs, from beginning to end, is the Layout." ]
            , p [] [ text "Think about a song that has a verse-chorus-verse pattern. It might have all of these arcs in this order: intro, verse, chorus, verse, chorus, chorus, verse, outro. So it has four unique arcs: intro, verse, chorus, and outro." ]
            ] 
          ] 
      , details []
          [ summary [Attr.style "cursor" "pointer", Attr.class "is-size-4"] [ text "Glossary" ]
          , div [ Attr.class "m-3" ]
            [  p [] [ text "The words we use to make songs, and what they mean." ]
            , h3 [] [ text "Song Details" ]
            , p [] [ text "Information about the song as a whole. These options are used on all of the song, not just individual arcs." ]
            , b [] [ text "Title" ]
            , p [] [ text "The name of your song. It does not affect the music." ]
            , b [] [ text "BPM" ]
            , p [] [ text "Beats Per Minute (BPM)"
                   , text "The speed of the song." ]
            , b [] [ text "Key" ]
            , p [] [ text "The key signature of the song." ]
            , h3 [] [ text "Arc" ]
            , p [] [ text "Information for this specific section of music." ]
            , b [] [ text "Title" ] 
            , p [] [ text "A label to help you remember what this Arc is. It does not affect the music." ]
            , b [] [ text "Size" ]
            , p [] [ text "How long this Arc is." ]
            , b [] [ text "Mix" ] 
            , p [] [ text "The instruments (beat or synths) that should go in this Arc." ]
            ]
       ]
  ]

welcome : Html msg
welcome = 
  div [ Attr.class "content" ]
    [ p [] [ text "Hi! I'm your Layout Designer. You can use me to build the layout of your song." ]
    , div [Attr.class "p-3" ] allTheDetails
    ]


showSequence : List Arc.Model -> Html msg
showSequence arcs =
  let 
    separator = "   ->   " 
    seq = List.foldl (\arc chain -> chain ++ arc.title ++ separator ) "" arcs
    show = String.dropRight (String.length separator) seq
  in
  div [Attr.class "px-3 my-3 has-text-centered"] [ text <| show ]


showUniques : List Arc.Model -> Html msg
showUniques arcs =
  let uniques = Group.uniques arcs in 
  div [] [
    div [Attr.class "columns is-multiline d-flex is-justify-content-center" ] <| List.map (\arc ->
      let count = List.foldl (\a sum -> if a == arc then sum + 1 else sum) 0 arcs in
      div [Attr.class "column is-narrow"] [ text <| (String.fromInt count) ++ "x", Arc.thumb arc ] ) uniques

   , if List.length uniques /= List.length arcs then 
     p [ Attr.class "mt-3 mb-6 p-3 has-background-success-light has-text-centered"] [ text "When you have two identical Arcs (the title, size, and style all match),"
           , br [] [] 
           , text "then the music for that Arc will be the same everywhere it is found in the Layout." ]
     else text ""
    ]

arcSummary : List Arc.Model -> Html msg
arcSummary arcs = 
  div []
    [ showUniques arcs
    ]

-- the summary shows you how many of each arc are present 
-- and a list of the names separated by arrows 

editor : Bool ->
  Bool ->
  Player.Model -> 
  (Player.Msg -> msg) -> 
  (String -> msg) -> 
  ScoreMeta.Model -> 
  msg -> 
  (Group.Msg Arc.Model  -> msg) -> 
  (Arc.Model -> msg) ->
  (List Arc.Model) -> 
  (ScoreMeta.Model -> (List Arc.Model) -> msg) ->  Html msg
editor anon isUsable player updatePlayer download meta editMeta editGroup openArc arcs doRequest =
  let
    nCycles = List.foldl (\card sum -> sum + (2 ^ card.size) ) 0 arcs 
  in
    Components.boxWith  (if isUsable then "" else "overlay-disabled")
      [ label [Attr.class "is-size-3 is-block mt-3 mb-6"] [ text "Title, BPM and Color" ]
      , ScoreMeta.readonly nCycles meta editMeta
      , label [Attr.class "is-size-3 is-block my-6"] [ text "Layout" ]
      , details [] [ summary [Attr.class "is-size-5"] [ text "Show Summary" ], arcSummary arcs ]
      , p [Attr.class "mt-3 mb-6" ] [ text "Use the buttons below to add, edit, remove, and position your Arcs." ]
      , showSequence arcs
      , Group.inserter editGroup Arc.empty (\i c -> Arc.stub c (openArc c))  arcs
      , if isUsable then 
          if List.length arcs > 0 && isUsable then Components.button (doRequest meta arcs) [Attr.class "mt-6 mb-3"] "Make a Song"
        else p  [] [ text "When you have at least 1 Arc in your layout, you can press the \"Make a Song\" button to produce the new music." ]
        else text ""
      , Player.view anon player updatePlayer download 
      ]


view : WithMember State -> 
  (Player.Msg -> msg) ->
  (String -> msg) -> 
  msg -> 
  (ScoreMeta.Msg -> msg) -> 
  (ScoreMeta.Model -> msg) ->  
  msg -> (Arc.Model -> msg) -> (Arc.Model -> msg) -> (Arc.Msg -> msg) -> (Arc.Model -> msg) -> msg -> msg -> (Group.Msg Arc.Model -> msg) 
  -> (ScoreMeta.Model -> (List Arc.Model) -> msg)
  -> Html msg
view (member, state) updatePlayer download editMeta changeMeta saveMeta closeMeta openArc editArc change save cancel createArc editGroup doRequest =
  div [] 
    [ welcome
    , h2 [Attr.class "is-size-2 my-6" ] [ text "Layout Designer"]
    , case state of  
      Requesting ({player, meta, arcs} as model) ->
        div [] 
          [ editor (isAnon member) False player updatePlayer download meta editMeta editGroup openArc (Tuple.second arcs) doRequest
          , p [ Attr.class "p-3 bg-info" ] [ text "Writing a song for you!" ]
          , p [ Attr.class "p-3 bg-info" ] [ text "This can take up to one minute." ]
          , p [ Attr.class "p-3 bg-info wait-a-minute" ] [ text "Looks like this song is taking longer; or maybe you lost network connection? Please try reloading the page, or contact us to report the issue." ]
          ]

      Viewing ({player, meta, arcs} as model) ->      
        editor (isAnon member) True player updatePlayer download meta editMeta editGroup openArc (Tuple.second arcs) doRequest

      EditingArc ({player, meta, arcs} as model) arcState -> 
        case arcState of 
          Arc.Viewing card -> Arc.readonly card (editArc card) cancel
          Arc.Editing orig next -> Arc.editor next change (save next) cancel

      EditingMeta ({player, meta, arcs} as model) metaState -> 
        case metaState of 
          ScoreMeta.Editing prev next ->
            ScoreMeta.editor next changeMeta (saveMeta next) closeMeta
          _ ->
            text "How did you get here"

    , div [Attr.class "box my-6 px-0 py-3"]
       [ div [ Attr.id "the-player"] [] ]
    ]


main = 
  Browser.element 
    { init = (\flags -> init flags GotTracks)
    , update = (\msg model -> update msg model GotTrack)
    , view = (\(member, state) -> view (member, state) UpdatePlayer Download EditMeta UpdateMeta SaveMeta CloseMeta ViewArc EditArc UpdateArc SaveArc CloseArc CreateArc EditGroup ReqTrack)
    , subscriptions = (\_ -> Sub.none)
    }
