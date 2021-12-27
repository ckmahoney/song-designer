module Router exposing (..)

import Browser
import Html exposing (Html, button, div, text, label, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Task
import Types exposing (..)
import Data
import View 
import Components
import Tools
import Array
import Http
import Url.Builder as Url
import Json.Decode as Decode
import Json.Encode as Encode

import ScopeEditor
import EnsembleEditor
import ComboEditor
import LayoutEditor
import Playback


-- data prepared for storage on server
type Msg 
  = UpdatePlayer (Playback.Player, Playback.Msg)
  | SelectTemplate Int
 
  | GotTracks (Result Http.Error (List TrackMeta))
  | GotNewTrack (Result Http.Error  TrackMeta)
  | ReqTrack Template
  | GotResp (Result Http.Error String)

  | Overview (List Combo)
  | EditingLayout (List Combo) Int LayoutEditor.Model
  
  | CloseLayoutEditor (List Combo)
  | UpdateTitle (String)
  | SaveLayout (List Combo)
  | OpenLayoutEditor (List Combo)
  | UpdateEditor (Maybe LayoutEditor.Model)


type alias Model =
  { response : String
  , mailer : Posting
  , tracks : List TrackMeta
  , selection : Maybe TrackMeta
  , playstate : Playback.Player
  , member : Maybe GhostMember
  , layout : List Combo
  , layoutEditor : Maybe LayoutEditor.Model
  , title : String
  , templates : List Template
  }


decodeTrackPrev : Decode.Decoder Track
decodeTrackPrev =
  Decode.map4 Track
    (Decode.field "id" Decode.int)
    (Decode.field "src" Decode.string)
    (Decode.field "size" Decode.int)
    (Decode.field "duration" Decode.float)


decodeTrack : Decode.Decoder TrackMeta
decodeTrack =
  Decode.map6 TrackMeta
    (Decode.field "id" Decode.int)
    (Decode.field "account_id" Decode.int)
    (Decode.field "filepath" Decode.string)
    (Decode.field "title" Decode.string)
    (Decode.field "size_bytes" Decode.int)
    (Decode.field "duration_seconds" Decode.float)


initFrom : List Voice -> List Scope -> List Layout -> List Template -> Maybe GhostMember -> Model
initFrom v s l t m =
 let
   template = Data.templateTernary
   layouts = [ Data.combos,  Tuple.second template ]
   layout = Tuple.second template
 in 
  Model  "" Welcome  [] Nothing Playback.new m layout  Nothing (Tuple.first template).title Data.templates


initTest : Model
initTest = 
  initFrom [Data.p1, Data.p2] [] [] [] (Just Data.testMember) 


initEmpty : Model
initEmpty = 
  Model  "" Welcome  [] Nothing Playback.new Nothing [] Nothing "Adventure Sound" []


initFromMember : GhostMember -> Model
initFromMember member = 
  let
    rec = { initEmpty | member = Just member }
  in 
  { initTest | member = Just member }


init : Maybe GhostMember -> (Model, Cmd Msg)
init flags =
  case flags of 
    Nothing -> 
     let
       member = Data.testMember
     in
      (initFromMember member, getSongs member.email member.uuid)
      -- (initEmpty, Cmd.none)
    
    Just member ->
      (initFromMember member, getSongs member.email member.uuid)


hostname = 
  "http://localhost:3000"
 -- "https://synthony.app"


apiUrl : String -> String 
apiUrl endpoint =
  Url.crossOrigin hostname [ endpoint ] []


reqTrack : String -> String -> Template -> Cmd Msg
reqTrack email uuid template =
  Http.post
    { url = apiUrl "track"
    , body =  Http.jsonBody <| encodeReqTrack email uuid template
    , expect = Http.expectJson GotNewTrack decodeTrack
    }


getSongs : String -> String -> Cmd Msg
getSongs email uuid =
  Http.post
  { url = apiUrl "user"
  , body = Http.jsonBody <| encodeReqLoadSongs email uuid
  , expect = Http.expectJson GotTracks (Decode.list decodeTrack)
  }


encodeScope : Scope -> Encode.Value
encodeScope {label, cps, cpc, root, size} =
  Encode.object
    [ ("label", Encode.string label)
    , ("cps", Encode.float cps)
    , ("root", Encode.float <| toFloat root)
    , ("cpc", Encode.int cpc)
    , ("size", Encode.int size)
    ]


encodeVoice : Voice -> Encode.Value
encodeVoice {duty, role, label, voice, density, complexity} =
  Encode.object
    [ ("duty", Encode.string <| Data.dutyString duty)
    , ("role", Encode.string <| Data.roleId role)
    , ("label", Encode.string label)
    , ("voice", Encode.int voice)
    , ("density", Encode.int density)
    , ("complexity", Encode.int complexity)
    ]


encodeEnsemble : Ensemble -> Encode.Value
encodeEnsemble  =
  Encode.list encodeVoice 


encodeScoreMeta : ScoreMeta -> Encode.Value
encodeScoreMeta {title, cps, root, cpc} =
  Encode.object
    [ ("title", Encode.string title)
    , ("cps", Encode.float cps)
    , ("root", Encode.float root)
    , ("cpc", Encode.int cpc)
    ]


encodeCombo : Combo -> Encode.Value
encodeCombo (scope, ensemble) =
  Encode.object
    [ ("scope", encodeScope scope) 
    , ("ensemble", encodeEnsemble ensemble)
    ]


encodeLayout : Layout -> Encode.Value
encodeLayout  combos =
  Encode.object
    [ ("title", Encode.string "ensemble")
    , ("combos", Encode.list encodeCombo combos)
    ]


encodeReqTrack : String -> String -> Template -> Encode.Value
encodeReqTrack email uuid template =
  Encode.object
    [ ("meta", encodeScoreMeta <| Tuple.first template)
    , ("layout", encodeLayout <| Tuple.second template)
    , ("email", Encode.string email)
    , ("uuid", Encode.string uuid)
    ]


encodeMember : GhostMember -> Encode.Value
encodeMember member =
  Encode.object
    [ ("name", Encode.string member.name)
    , ("email", Encode.string member.email)
    , ("uuid", Encode.string member.uuid)
    ]


encodeUserReq :  Encode.Value
encodeUserReq  =  Encode.object
    [ ("action", Encode.string "songs")
    , ("username", Encode.string "maxwell")
    ]


encodeReqLoadSongs : String -> String -> Encode.Value
encodeReqLoadSongs email uuid = Encode.object
    [ ("action", Encode.string "songs")
    , ("email", Encode.string email)
    , ("uuid", Encode.string uuid)
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Overview state ->
      (model, Cmd.none)

    EditingLayout layout index mod  ->
      ({ model | layoutEditor = Just <| LayoutEditor.Overview layout}, Cmd.none)

    UpdateTitle title ->
      ({ model | title = title }, Cmd.none)

    CloseLayoutEditor layout ->
      ({ model | layout =  layout, layoutEditor = Nothing }, Cmd.none)

    OpenLayoutEditor layout ->
      ({ model | layoutEditor = Just <| LayoutEditor.Overview layout } , Cmd.none)

    UpdateEditor editor ->
      ({ model | layoutEditor = editor  } , Cmd.none)

    SaveLayout layout ->
      ({ model | layout = layout }, Cmd.none)

    UpdatePlayer ((mTrack, pstate) as playstate, pCmd) ->
      case mTrack of
        Nothing ->
         ({ model | playstate = Playback.new}, Playback.stopMusic "")

        Just t ->
           case pCmd of 
            Playback.Load path -> ( { model | playstate = playstate }, Playback.trigger <| Playback.Load (hostname ++ path))
            Playback.Select (Just track) ->    
             ( { model | playstate = playstate }, Playback.trigger <| Playback.Load (hostname ++ track.filepath)) -- fixes the missing hostname on getSongs

            _ ->
             ( { model | playstate = playstate }, Playback.trigger pCmd)

    GotTracks response ->
      case response of 
        Ok tracks ->
         ({ model | mailer = Received, tracks = tracks }, Cmd.none)

        Err errr ->
          case errr of 
            Http.BadBody str -> 
              ({ model | mailer = Failed str }, Cmd.none)
 
            Http.BadUrl str -> 
              ({ model | mailer = Failed str }, Cmd.none)

            Http.BadStatus int -> 
              ({ model | mailer = Failed <| String.fromInt int }, Cmd.none)

            _ -> 
              ({ model | mailer = Failed "big bug" }, Cmd.none)

    GotNewTrack response ->
      case response of 
        Ok track ->
          ( { model 
            | tracks = track :: model.tracks
            , selection = (Just track)
            , playstate = (Just track, Playback.Playing)
            , mailer = Received }, Playback.trigger <| Playback.Load (hostname ++ track.filepath ) )

        Err errr ->
          case errr of 
            Http.BadUrl str -> 
              ({ model | mailer = Failed str }, Cmd.none)

            Http.BadStatus int -> 
              ({ model | mailer = Failed <| String.fromInt int }, Cmd.none)

            Http.BadBody str -> 
              ({ model | mailer = Failed str }, Cmd.none)
 
            _ -> 
              ({ model | mailer = Failed "big bug" }, Cmd.none)

    ReqTrack template -> 
      case model.member of 
        Nothing -> 
         let
          member = Data.testMember
         in
          ( { model | mailer = Sending }, reqTrack member.email member.uuid template )
          -- ( model, Cmd.none )

        Just member ->
          ( { model | mailer = Sending }, reqTrack member.email member.uuid template )

    GotResp result -> 
      case result of 
        Ok str -> 
          ({ model | response = str }, Cmd.none)

        Err _ -> 
          ({ model | response = "We had a problem getting your string." }, Cmd.none)

    SelectTemplate index ->
      let
        ((meta, lay) as t) = Tools.getOr index model.templates Data.emptyTemplate
      in 
      ( {model | layout = lay, title = meta.title}, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


toText : Int -> Html msg
toText x =
  text <| String.fromInt x


listLenText : List a -> String
listLenText xs = 
  String.fromInt <| List.length xs


panel : String -> String -> List a -> msg -> Html msg
panel name plural coll msg =
  div [ class "column" ] [ Components.card name <| 
    div [] 
    [ p [] [ text ("You have " ++ (listLenText coll) ++ " " ++ plural) ] 
    , Components.button msg [] ( name ++ " Overview")
    ] ]

introduceVoice : Html msg -> msg -> Html msg
introduceVoice helpLink create =
  Components.box 
    [ p [ class "label" ] [ text "You need some voices to make a template. We recommend 4 unique voices to get started." ]
     , Components.cols 
      [ Components.colHalf <| Components.wraps  
        [ p [] [ text "Do you want some help making your first voices and template? Click here for the guide." ]
        , helpLink ] 
      , Components.colHalf <| Components.wraps 
         [ p [] [ text "Ready to get going? Click this button to get started now." ] 
        , Components.button create [] "Make a Voice"
     ] ] ]


introduceScope : Html msg -> msg -> Html msg
introduceScope helpLink create =
  Components.box 
    [ p [ class "label" ] [ text "Scopes let us build Layouts. We recommend 2 unique scopes to get started, but you can always make as many as you want." ]
     , Components.cols 
      [ Components.colHalf <| Components.wraps  
        [ p [] [ text "Do you want some help making your first scope and layout? Click here for the guide." ]
        , helpLink ] 
      , Components.colHalf <| Components.wraps 
         [ p [] [ text "Ready to get going? Click this button to get started now." ] 
        , Components.button create [] "Make a Scope"
     ] ] ]


introduceLayout : Html msg -> msg -> Html msg
introduceLayout helpLink create =
  Components.box 
    [ p [ class "label" ] [ text "Layouts are the fundamental structure of your composition." ]
     , Components.cols 
      [ Components.colHalf <| Components.wraps  
        [ p [] [ text "We recommend everybody read the guide for Layouts before making one." ]
        , helpLink ] 
      , Components.colHalf <| Components.wraps 
         [ p [] [ text "Ready to get going? Click this button to get started now." ] 
        , Components.button create [] "Make a Layout"
     ] ] ]


introduceTemplate : Html msg -> msg -> Html msg
introduceTemplate helpLink create =
  Components.box 
    [ p [ class "label" ] [ text "Templates are the seeds for your flowers of music." ]
    , p [ class "label" ] [ text "Every template can produce an infinite number of songs, where each song similar to its siblings in some ways while being unique." ]
     , Components.cols 
      [ Components.colHalf <| Components.wraps  
        [ p [] [ text "We recommend everybody read the guide for Templates before making one." ]
        , helpLink ] 
      , Components.colHalf <| Components.wraps 
         [ p [] [ text "Ready to get going? Click this button to get started now." ] 
        , Components.button create [] "Make a Template"
     ] ] ]


clear msg =
  msg -1 Nothing


updateIn : a -> List a -> Int -> List a 
updateIn el els index =
  Tools.replaceAt index el els


requestSongButton : Model -> Html Msg
requestSongButton model =
  if 0 == List.length model.layout then 
     text ""
  else 
    let 
       ref = Data.scoreMetaT0
       meta = { ref | title = model.title }
       template : Template
       template = ({meta | title = model.title},  model.layout)
    in  
     Components.button (ReqTrack template) [ class "is-primary is-size-4 p-3"] ("Write A New Song: " ++ model.title )


editLayoutButton : Model -> Html Msg
editLayoutButton model =
  Components.button (OpenLayoutEditor model.layout) [class "is-size-4 is-info mb-3"] "Edit this Template"


miniSongDesigner : Model -> Html Msg
miniSongDesigner model =
  div [class "box"] <| 
   [ case model.layoutEditor of 
      Nothing -> 
       div []
         [ Html.h2 [class "title"] [text "Song Designer"]
         , templatePicker model.templates 
         , Components.cols <| 
             [ Components.col1 <| editLayoutButton model
             , Components.col1 <| requestSongButton model
             ]
         , div [] <| LayoutEditor.look model.layout
         ]

      Just mod ->
        div []
        [ LayoutEditor.view model.title mod (\m -> UpdateEditor <| Just m) UpdateTitle SaveLayout CloseLayoutEditor 
        ]
   ]

templateIcon : msg -> Template ->  Html msg 
templateIcon pick ((meta, lay) as template)  =
 let
  scopes = List.map Tuple.first lay
  totalLength = View.totalLength scopes
 in 
   Components.box
    [ p [class "subtitle is-size-4"] [text meta.title]
    , p [] [text <| (String.fromInt (List.length lay)) ++ " combos"]
    , p [] [text <| View.timeString totalLength]
    , Components.svg "select" 
    ]


templatePicker : List Template -> Html Msg
templatePicker layouts = 
  Components.colsMulti <|
    [ Components.colFull <| Html.h3 [class "is-size-3"] [text "Select a Preset"]
    , Components.colFull <| Components.colsMulti <| 
      List.indexedMap (\i template -> 
        Components.col [onClick (SelectTemplate i), Components.centerText] [templateIcon (SelectTemplate i) template]) layouts
    ]

view : Model -> Html Msg
view model =
    div [ class "section" ]
      [ case model.member of 
          Nothing -> text ""
          Just m -> Html.h2 [class "subtitle"] [text ("Welcome back " ++ m.firstname)]

      , case model.mailer of 
          Sending -> 
            text "Working on that track for you!"
          _ ->
            miniSongDesigner model 

      , Playback.view model.playstate UpdatePlayer model.tracks
      ]


main =  Browser.element 
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }
