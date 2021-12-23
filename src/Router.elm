port module Router exposing (..)

import Browser
import Html exposing (Html, button, div, text, label, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Types as T
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


port playMusic : String -> Cmd msg

port pauseMusic : String -> Cmd msg

port stopMusic : String -> Cmd msg

port setSource : String -> Cmd msg




type Playback
 = Play
 | Pause
 | Stop  


-- data prepared for storage on server
type Msg 
  = LoadTrack T.TrackMeta
  | PlayTrack 
  | PauseTrack
  | StopTrack
  | SelectTrack (Maybe T.TrackMeta)
 
  | GotTracks (Result Http.Error (List T.TrackMeta))
  | GotNewTrack (Result Http.Error  T.TrackMeta)
  | ReqTrack T.Template
  | GotResp (Result Http.Error String)


  | Overview (List T.Combo)
  | EditingLayout (List T.Combo) Int LayoutEditor.Model
  
  | OpenLayoutEditor (List T.Combo)
  | CloseLayoutEditor (List T.Combo)
  | ChangeLayoutEditor (List T.Combo)
  | SaveLayoutEditor  (List T.Combo)
  | SelectLayoutEditor (List T.Combo) Int T.Combo
  | UpdateLayoutEditor LayoutEditor.Msg
  | UpdateTitle (String)


changeLayout : Model -> (List T.Combo) -> Model
changeLayout model layout = 
  { model | layout = layout }


type alias Model =
  { response : String
  , mailer : T.Posting
  , tracks : List T.TrackMeta
  , selection : Maybe T.TrackMeta
  , playstate : Playback
  , member : Maybe T.GhostMember
  , layout : List T.Combo
  , layoutEditor : Maybe LayoutEditor.Model
  , title : String
  }




-- -- access scope
-- (\layout i-layout scope)

-- -- create scope
-- (\layout i-layout ensemble i-ensemble)  

-- -- delete scope
-- (\layout i-layout ensemble i-ensemble)  

-- -- update scope
-- (\layout i-layout ensemble i-ensemble scope field)  


-- -- create voice
-- (\layout i-layout ensemble i-ensemble)  

-- -- delete voice
-- (\layout i-layout ensemble i-ensemble)  

-- -- access voice
-- (\layout i-layout ensemble i-ensemble voice)  

-- -- update voice
-- (\layout i-layout ensemble i-ensemble voice field)  


-- updateLayoutScope layout iLayout scope iEnsemble scope field)   =
-- LayoutEditor.Update iLayout 
  -- <| ComboEditor.UpdateScope scope

decodeTrackPrev : Decode.Decoder T.Track
decodeTrackPrev =
  Decode.map4 T.Track
    (Decode.field "id" Decode.int)
    (Decode.field "src" Decode.string)
    (Decode.field "size" Decode.int)
    (Decode.field "duration" Decode.float)


decodeTrack : Decode.Decoder T.TrackMeta
decodeTrack =
  Decode.map6 T.TrackMeta
    (Decode.field "id" Decode.int)
    (Decode.field "account_id" Decode.int)
    (Decode.field "filepath" Decode.string)
    (Decode.field "title" Decode.string)
    (Decode.field "size_bytes" Decode.int)
    (Decode.field "duration_seconds" Decode.float)


initFrom : List T.Voice -> List T.Scope -> List T.Layout -> List T.Template -> Maybe T.GhostMember -> Model
initFrom v s l t m =
  Model  "" T.Welcome  [] Nothing Stop m LayoutEditor.initState Nothing ""


initTest : Model
initTest = 
  initFrom [Data.p1, Data.p2] [] [] []  Nothing


initEmpty : Model
initEmpty = 
  Model  "" T.Welcome  [] Nothing Stop Nothing LayoutEditor.initState Nothing "Adventure Sound"


initFromMember : T.GhostMember -> Model
initFromMember member = 
  let
    rec = { initEmpty | member = Just member }
  in 
  { initTest | member = Just member }


init : Maybe T.GhostMember -> (Model, Cmd Msg)
init flags =
  case flags of 
    Nothing -> 
      (initEmpty, Cmd.none)
    
    Just member ->
      (initFromMember member, getSongs member.email member.uuid)




hostname = 
  -- "http://localhost:3000/"
  "https://synthony.app"


apiUrl : String -> String 
apiUrl endpoint =
  Url.crossOrigin hostname [ endpoint ] []


reqTrack : String -> String -> T.Template -> Cmd Msg
reqTrack email uuid template =
  Http.post
    { url = apiUrl "track"
    , body =  Http.jsonBody <| encodeReqTrack email uuid template
    , expect = Http.expectJson GotNewTrack decodeTrack
    }

reqMiniTrack : String -> String -> T.Layout -> Cmd Msg
reqMiniTrack email uuid ((title, combos) as layout) =
 let   
  -- meta = {Data.scoreMetaT0 | title = title }
  template = (Data.scoreMetaT0, layout)
 in
  Http.post
  { url = apiUrl "track"
  , body = Http.jsonBody <| encodeReqTrack email uuid template
  , expect = Http.expectJson GotNewTrack decodeTrack
  }


getSongs : String -> String -> Cmd Msg
getSongs email uuid =
  Http.post
  { url = apiUrl "user"
  , body = Http.jsonBody <| encodeReqLoadSongs email uuid
  , expect = Http.expectJson GotTracks (Decode.list decodeTrack)
  }


encodeScope : T.Scope -> Encode.Value
encodeScope {label, cps, cpc, root, size} =
  Encode.object
    [ ("label", Encode.string label)
    , ("cps", Encode.float cps)
    , ("root", Encode.float <| toFloat root)
    , ("cpc", Encode.int cpc)
    , ("size", Encode.int size)
    ]


encodeVoice : T.Voice -> Encode.Value
encodeVoice {duty, role, label, voice, density, complexity} =
  Encode.object
    [ ("duty", Encode.string <| Data.dutyString duty)
    , ("role", Encode.string <| Data.roleId role)
    , ("label", Encode.string label)
    , ("voice", Encode.int voice)
    , ("density", Encode.int density)
    , ("complexity", Encode.int complexity)
    ]


encodeEnsemble : T.Ensemble -> Encode.Value
encodeEnsemble  =
  Encode.list encodeVoice 


encodeScoreMeta : T.ScoreMeta -> Encode.Value
encodeScoreMeta {title, cps, root, cpc} =
  Encode.object
    [ ("title", Encode.string title)
    , ("cps", Encode.float cps)
    , ("root", Encode.float root)
    , ("cpc", Encode.int cpc)
    ]


encodeCombo : T.Combo -> Encode.Value
encodeCombo (scope, ensemble) =
  Encode.object
    [ ("scope", encodeScope scope) 
    , ("ensemble", encodeEnsemble ensemble)
    ]


encodeLayout : T.Layout -> Encode.Value
encodeLayout (label, combos) =
  Encode.object
    [ ("title", Encode.string label)
    , ("combos", Encode.list encodeCombo combos)
    ]


encodeReqTrack : String -> String -> T.Template -> Encode.Value
encodeReqTrack email uuid template =
  Encode.object
    [ ("meta", encodeScoreMeta <| Tuple.first template)
    , ("layout", encodeLayout <| Tuple.second template)
    , ("email", Encode.string email)
    , ("uuid", Encode.string uuid)
    ]


encodeMember : T.GhostMember -> Encode.Value
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

    OpenLayoutEditor layout ->
      ({ model | layoutEditor = Just <| LayoutEditor.Overview layout } , Cmd.none)
    UpdateTitle title ->
      ({ model | title = title }, Cmd.none)

    ChangeLayoutEditor layout ->
      ({ model | layout = layout, layoutEditor = Just <| LayoutEditor.Overview layout }, Cmd.none)

    CloseLayoutEditor layout ->
      ({ model | layout = layout, layoutEditor = Nothing }, Cmd.none)

    SaveLayoutEditor layout ->
      ({ model |  layout = layout, layoutEditor = Just <| LayoutEditor.Overview layout }, Cmd.none)

    SelectLayoutEditor layout index combo ->
      ({ model | layoutEditor = Just <| LayoutEditor.update (LayoutEditor.Select index) layout }, Cmd.none)

    UpdateLayoutEditor sMsg ->
     let 
       mod = Debug.log "UPdating layout:" LayoutEditor.update sMsg model.layout
     in 
     case sMsg of 
      LayoutEditor.Save next -> ( { model | layout = next, layoutEditor = Just <| LayoutEditor.Overview next }, Cmd.none)
      _ -> ({ model | layoutEditor = Just <| LayoutEditor.update sMsg model.layout }, Cmd.none)

      -- LayoutEditor.Overview next -> ( { model | layout = next, layoutEditor = Just <| LayoutEditor.Overview next  }, Cmd.none)
      -- LayoutEditor.Editing _ _ _  -> ( { model | layoutEditor = Just mod }, Cmd.none)

    LoadTrack track -> 
      ( model, setSource track.filepath )

    SelectTrack track ->
      case track of 
        Nothing -> 
          ( { model | playstate = Stop, selection = Nothing }, setSource "" )
        Just t -> 
          ( { model | playstate = Play, selection = Just t }, setSource (hostname ++ t.filepath ))

    PlayTrack ->
      ( { model | playstate = Play } , playMusic "" )

    PauseTrack ->
      ( { model | playstate = Pause } , pauseMusic "" )

    StopTrack ->
      ( { model | playstate = Stop, selection = Nothing }, stopMusic "" )

    GotTracks response ->
      case response of 
        Ok tracks ->
         ({ model | mailer = T.Received, tracks = tracks }, Cmd.none)

        Err errr ->
          case errr of 
            Http.BadBody str -> 
              ({ model | mailer = T.Failed str }, Cmd.none)
 
            Http.BadUrl str -> 
              ({ model | mailer = T.Failed str }, Cmd.none)

            Http.BadStatus int -> 
              ({ model | mailer = T.Failed <| String.fromInt int }, Cmd.none)

            _ -> 
              ({ model | mailer = T.Failed "big bug" }, Cmd.none)

    GotNewTrack response ->
      case response of 
        Ok track ->
          ( { model 
            | tracks = track :: model.tracks
            , selection = (Just track)
            , playstate = Play
            , mailer = T.Received }, setSource (hostname ++ track.filepath ) )

        Err errr ->
          case errr of 
            Http.BadUrl str -> 
              ({ model | mailer = T.Failed str }, Cmd.none)

            Http.BadStatus int -> 
              ({ model | mailer = T.Failed <| String.fromInt int }, Cmd.none)

            Http.BadBody str -> 
              ({ model | mailer = T.Failed str }, Cmd.none)
 
            _ -> 
              ({ model | mailer = T.Failed "big bug" }, Cmd.none)

    ReqTrack template -> 
      case model.member of 
        Nothing -> 
         let
          member = Data.testMember
         in
          ( { model | mailer = T.Sending }, reqTrack member.email member.uuid template )
          -- ( model, Cmd.none )

        Just member ->
          ( { model | mailer = T.Sending }, reqTrack member.email member.uuid template )

    GotResp result -> 
      case result of 
        Ok str -> 
          ({ model | response = str }, Cmd.none)

        Err _ -> 
          ({ model | response = "We had a problem getting your string." }, Cmd.none)



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


playlist : Playback -> (Maybe T.TrackMeta) -> List T.TrackMeta -> Html Msg
playlist playstate selection tracks =
  Components.box <| List.singleton  <| Components.colsMulti <|
   List.map (\track ->
     let 
       icons = case selection of 
         Nothing -> 
            [ div [onClick <| SelectTrack (Just track)] [Components.svg "play"] ]

         Just selected ->  
           if selected == track then 
             [ case playstate of 
                 Play -> 
                   div [onClick PauseTrack] [Components.svg "pause"] 

                 Pause -> 
                   div [onClick PlayTrack] [Components.svg "play"] 

                 Stop ->
                   div [onClick PlayTrack] [Components.svg "play"] 

             , div [onClick StopTrack] [Components.svg "stop"]
             ]
           else 
             [ div [onClick <| SelectTrack (Just track)] [Components.svg "play"] ]

     in
     Components.col [class "is-one-fifth"] [ Components.songCard track.title icons]) tracks


requestSongButton : Model -> Html Msg
requestSongButton model =
  if 0 == List.length model.layout then 
     text ""
  else 
    let 
       ref = Data.scoreMetaT0
       meta = { ref | title = model.title }
       template = (meta, ("Needs a title", model.layout))
    in  
     Components.button (ReqTrack template) [style "width" "100%", class "is-primary mb-3"] ("Write A New Song: " ++ model.title )


editLayoutButton : Model -> Html Msg
editLayoutButton model =
  Components.button (OpenLayoutEditor model.layout) [style "width" "100%", class "is-info mb-3"] "Edit your layout"



prevEditor model lModel =
 case lModel of 
  LayoutEditor.Overview layout -> 
   let 
     addAnother = (OpenLayoutEditor <| List.reverse <| Data.emptyCombo :: layout)
   in
   div [] 
    [ Components.editText "Title" (text "The name for this sound") model.title UpdateTitle
    -- , LayoutEditor.view layout (\i -> 
        -- SelectLayoutEditor layout i <| Tools.getOr i layout Data.emptyCombo) (\i -> OpenLayoutEditor (Tools.removeAt i layout)) addAnother
    , Components.button (CloseLayoutEditor layout) [] " Make a Song"
    ]

  LayoutEditor.Editing layout index stateModel -> 
   let
     up = UpdateLayoutEditor 
     combo = Tools.getOr index layout Data.emptyCombo
     done = OpenLayoutEditor layout
   in
    div []
      [ div [class "mb-6"] [ Components.button (CloseLayoutEditor layout) [] "Save Layout" ]
      -- , LayoutEditor.edit stateModel index combo up done
      ]


miniSongDesigner : Model -> Html Msg
miniSongDesigner model =
  div [class "has-background-light"] <| List.singleton <|
    case model.layoutEditor of 
      Nothing -> 
       Components.box <|
         [ editLayoutButton model
         , requestSongButton model
         , div [] <| LayoutEditor.look model.layout
         ]

      Just lModel ->
        LayoutEditor.viewNew lModel UpdateLayoutEditor (ChangeLayoutEditor) (CloseLayoutEditor model.layout)


view : Model -> Html Msg
view model =
    div [ class "section" ]
      [ case model.member of 
          Nothing -> text ""
          Just m -> Html.h2 [class "subtitle"] [text ("Welcome back " ++ m.firstname)]
      -- , menu model.view
      , case model.mailer of 
          T.Sending -> 
            text "Working on that track for you!"
          _ ->
            miniSongDesigner model
      

            -- display model Select 
      -- , text model.response
      , playlist model.playstate model.selection model.tracks
      ]


main =  Browser.element 
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }
