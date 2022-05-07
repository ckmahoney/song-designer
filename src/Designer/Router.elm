module Router exposing (..)

import Browser
import Html exposing (Html, button, div, text, label, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Defs.Types exposing (..)
import Defs.Data
import Components.View as View 
import Components.Components as Components
import Tools
import Http
import Url.Builder as Url
import Json.Decode as Decode
import Json.Encode as Encode
import Encoders as JE
import Defs.Configs as Conf

import Editor.Scope
import Editor.Ensemble
import Editor.Combo
import Editor.Layout
import Comm.Playback


type alias ID = Int


type Msg 
  = UpdatePlayer (Playback.Model, Playback.Msg)
  | SelectTemplate Int
 
  | GotTracks (Result Http.Error (List TrackMeta))
  | GotNewTrack (Result Http.Error TrackMeta)
  | GotAsset (Result Http.Error String)
  | ReqTrack Template
  | ReqAsset ID Playback.Asset

  | Overview (List Combo)
  | EditingLayout (List Combo) Int LayoutEditor.Model
  
  | CloseLayoutEditor (List Combo)
  | UpdateTitle (String)
  | SaveLayout (List Combo)
  | OpenLayoutEditor (List Combo)
  | UpdateEditor (Maybe LayoutEditor.Model)
  | Download String


type alias Model =
  { mailer : Posting
  , tracks : List TrackMeta
  , selection : Maybe TrackMeta
  , playback : Playback.Model
  , member : Maybe GhostMember
  , layout : List Combo
  , layoutEditor : Maybe LayoutEditor.Model
  , title : String
  , templates : List Template
  }


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
   template = Data.templateBright
   layouts = [ Data.combos,  Tuple.second template ]
   layout = Tuple.second template
 in 
  Model  Welcome  [] Nothing Playback.new m layout  Nothing (Tuple.first template).title Data.templates


initTest : Maybe GhostMember -> (Model, Cmd msg)
initTest flags = 
  let
    rec = initFrom [Data.p1, Data.p2] [] [] [] (Just Data.testMember)
  in 
    ({ rec
    | tracks = Data.someTracks
    , member = Just Conf.anonMember
    },
    Cmd.none
    )


initWithDefaults : Model
initWithDefaults = 
  initFrom [Data.p1, Data.p2] [] [] [] (Just Data.testMember) 


initEmpty : Model
initEmpty = 
  let
    rec =   Model  Welcome  [] Nothing Playback.new Nothing [] Nothing "Adventure Sound" []
  in 
  -- rec 
  { initWithDefaults | member = Just Conf.anonMember }


initFromMember : GhostMember -> Model
initFromMember member = 
  let
    rec = { initEmpty | member = Just member }
  in 
  { initWithDefaults | member = Just member }


init : Maybe GhostMember -> (Model, Cmd Msg)
init flags =
  case flags of 
    Nothing -> 
      (initEmpty, Cmd.none)
    
    Just member ->
      (initFromMember member, getSongs member.email member.uuid)


apiUrl : String -> String 
apiUrl endpoint =
  Url.crossOrigin Conf.hostname [ endpoint ] []


reqTrack : String -> String -> Template -> Cmd Msg
reqTrack email uuid template =
  Http.post
    { url = apiUrl "track"
    , body =  Http.jsonBody <| JE.encodeReqTrack email uuid template
    , expect = Http.expectJson GotNewTrack decodeTrack
    }


reqAsset : String -> String -> Int -> Playback.Asset -> Cmd Msg
reqAsset email uuid id kind =
  Http.post
    { url = apiUrl "user"
    , body =  Http.jsonBody <| encodeReqAsset email uuid id kind
    , expect = Http.expectString GotAsset
    }


getSongs : String -> String -> Cmd Msg
getSongs email uuid =
  Http.post
  { url = apiUrl "user"
  , body = Http.jsonBody <| encodeReqLoadSongs email uuid
  , expect = Http.expectJson GotTracks (Decode.list decodeTrack)
  }


encodeReqAsset : String -> String -> Int -> Playback.Asset -> Encode.Value
encodeReqAsset email uuid id kind =
  Encode.object
    [ ("action", Encode.string "asset")
    , ("id", Encode.int id)
    , ("type", Encode.string <| Playback.assetName kind)
    , ("email", Encode.string email)
    , ("uuid", Encode.string uuid)
    ]

encodeUserReq :  Encode.Value
encodeUserReq  =  
  Encode.object
    [ ("action", Encode.string "songs")
    , ("username", Encode.string "maxwell")
    ]


encodeReqLoadSongs : String -> String -> Encode.Value
encodeReqLoadSongs email uuid = 
  Encode.object
    [ ("action", Encode.string "songs")
    , ("email", Encode.string email)
    , ("uuid", Encode.string uuid)
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Download url ->
      (model, Conf.download url)

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

    UpdatePlayer (playback, pMsg) ->
      let
        ((updated, pMsg_) as result) = Playback.update pMsg playback
        next = { model | playback = updated }
      in 
              
      case Tuple.first updated of
        Nothing ->
         ( { next
           | selection = Nothing
           }
         , Playback.trigger pMsg_
         )

        Just track ->
         ( { next
           | selection = Just track
           }
         , Playback.trigger pMsg_
         )

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
            , playback = (Just track, Playback.Playing)
            , mailer = Received 
            }
            , Playback.trigger <| Playback.Load <|  Conf.hostname ++ track.filepath 
            )

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


    GotAsset response ->
      case response of 
        Ok url ->
          ( model, Conf.download url )

        Err errr ->
          case errr of 
            Http.BadUrl str -> 
              ({ model | mailer = Failed str }, Cmd.none)

            Http.BadStatus int -> 
              ({ model | mailer = Failed <| String.fromInt int }, Cmd.none)

            Http.BadBody str -> 
              ({ model | mailer = Failed str }, Cmd.none)
 
            _ -> 
              ({ model | mailer = Failed "Sorry, we had an oops while making that asset for you. Can you please tell us about it so we can fix it for you?" }, Cmd.none)


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


    SelectTemplate index ->
      let
        ((meta, lay) as t) = Tools.getOr index model.templates Data.emptyTemplate
      in 
      ( {model | layout = lay, title = meta.title}, Cmd.none)


    ReqAsset id kind ->
      case model.member of 
        Nothing -> 
         let
          member = Data.testMember
         in
          ( { model | mailer = Sending }, reqAsset member.email member.uuid id kind )
          -- ( model, Cmd.none )

        Just member ->
          ( { model | mailer = Sending }, reqAsset member.email member.uuid id kind )
      

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
     Components.button (ReqTrack template) [ class "is-primary is-size-4 p-3"] ("Make A New Song: " ++ model.title )


editLayoutButton : Model -> Html Msg
editLayoutButton model =
  Components.button (OpenLayoutEditor model.layout) [class "is-size-4 is-info mb-3"] "Edit this Template"


songDesigner : Model -> Html Msg
songDesigner model =
  div [class "box"] <| 
   [ case model.layoutEditor of 
      Nothing -> 
       div []
         [ Html.h2 [class "title"] [text "Song Designer"]
         , templatePicker model.templates 
         , div [class " box is-block has-text-centered" ] [text "Current template:", Html.h2 [class "is-size-1"] [text model.title]  ]
         , Components.colsWith [class "is-mobile"] <| 
             [ Components.col1 <| editLayoutButton model
             , Components.col1 <| requestSongButton model
             ]
         , Components.colsMulti <| LayoutEditor.look model.layout
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
    , div [class "is-clickable"] [ Components.svg "select" ]
    ]


templatePicker : List Template -> Html Msg
templatePicker layouts = 
  Components.colsMulti <|
    [ Components.colFull <| Html.h3 [class "is-size-5"] [text "Choose a Preset"]
    , Components.colFull <| Components.colsWith [class "is-mobile"] <| 
      List.indexedMap (\i template -> 
        Components.col [onClick (SelectTemplate i), Components.centerText] [templateIcon (SelectTemplate i) template]) layouts
    ]


isActiveMember : Model -> Bool
isActiveMember model =
  if Conf.devMode then True else 
  case model.member of 
    Nothing -> False
    Just  member -> 
      if member == Conf.anonMember then False
      else True


joinCTA : Html msg
joinCTA =
  div [] 
    [ div [class "notification is-warning is-light"]
        [ text "Our song designer is for members only." ]
    ,  div [class "notification is-success is-light"]
        [ text  "Good news for you, joining is easy and free!"]
    ]


view : Model -> Html Msg
view model =
  let
    classname = if isActiveMember model
        then ""
        else "overlay-disabled"
  in 
    div [ class "section" ]
      [ if isActiveMember model
          then case model.member of  
            Just m -> Html.h2 [class "subtitle"] [text ("Welcome back " ++ m.firstname)]
            _ -> text ""
          else 
            div [class "my-6 has-text-centered is-size-3"] [text "Sign up to make new songs with the Song Designer."]
      
      , div [class classname]
        [ if isActiveMember model 
            then Playback.view model.playback UpdatePlayer ReqAsset model.tracks Download
            else Components.paragraph "Log in to load your Song Bank."
        , case model.mailer of 
            Sending -> 
              text "Making that new music for you!"
            _ ->
              songDesigner model 
        ]
      ]


main =  Browser.element 
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }
