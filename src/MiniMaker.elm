module MiniMaker exposing (Model, view)
-- Instance of a song maker with limited options

import Browser exposing (element)
import Html exposing (Html, div, p, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Random
import Url.Builder as Url
import Json.Decode as Decode
import Json.Encode as Encode

import Types exposing (SynthRole(..), ScoreMeta, TrackMeta, Template)
import Data exposing (synthRoles)
import Components
import View
import Tools


type Speed 
  = Slow
  | Medium 
  | Fast


type alias Model =
  { title : Maybe String
  , speed : Speed
  , voices: List SynthRole
  , tracks : List TrackMeta
  , error : Maybe String
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


type Msg 
  = SetTitle String
  | SetSpeed Speed
  | ToggleVoice SynthRole
  | PushedButton

  | RollTemplate Model
  | GotTrack (Result Http.Error TrackMeta)
 

apiUrl : String -> String 
apiUrl endpoint =
  Url.crossOrigin hostname [ endpoint ] []


hostname = 
  "http://localhost:3000"
 -- "https://synthony.app"


miniMakerMember =
  { uuid = ""
  , email = ""
  }


rollTexture : Random.Generator Int
rollTexture = 
  Random.int 1 3


rollCps : Random.Generator Float
rollCps = 
  Random.int 3 4


-- the Elm app right now sends requests using 0-11 semitone notation instead of Hz values
rollRoot : Random.Generator Float
rollRoot = 
  Random.map toFloat <| Random.int 0 11


rollCpc : Random.Generator Int
rollCpc = 
  Random.int 3 4


rollMeta : String -> Random.Generator ScoreMeta
rollMeta title = 
  Random.map3 (ScoreMeta title) rollCps rollRoot rollCpc

modelToLayout : Model -> Layout
modelToLayout state =
 let
   scope = 
   ensemble = List.map
 in
  [ scope, ensemble  ]  


modelToTemplate : Model -> Cmd Msg
modelToTemplate model =
  Random.generate RollTemplate model
  

reqTrack : String -> String -> Model -> Cmd Msg
reqTrack email uuid state =
  Http.post
    { url = apiUrl "track"
    , body =  Http.jsonBody <| encodeReqTrack email uuid template
    , expect = Http.expectJson GotTrack decodeTrack
    }


-- I need to create a template of length 1 


encodeReqTrack : String -> String -> Model -> Encode.Value
encodeReqTrack email uuid state =
  Encode.object
    [ ("meta", encodeScoreMeta <| Tuple.first template)
    , ("layout", encodeLayout <| Tuple.second template)
    , ("email", Encode.string email)
    , ("uuid", Encode.string uuid)
    ]



initModel : Model
initModel =
  { title = Just ""
  , speed = Medium
  , voices = [ Kick, Hat, Melody ]
  , tracks = []
  , error = Nothing
  }


init : Maybe Int -> (Model, Cmd Msg)
init _ =
  (initModel, Cmd.none)


validReq : Model -> Bool
validReq state = 
  case state.title of 
    Nothing -> False
    Just "" -> False
    Just title -> 
      List.length state.voices > 0 


apply : Msg -> Model -> Model
apply msg model =
  case msg of 
    GotTrack response ->
      case response of 
        Ok track ->
         { model | tracks = track :: model.tracks }

        Err errr ->
          case errr of 
            Http.BadBody str -> 
              { model | error = Just "How did you post that body?"  }
 
            Http.BadUrl str -> 
              { model | error = Just "Where did you get that URL?" }

            Http.BadStatus int -> 
              { model | error = Just <| "The server looks like it had a problem. Here's a hint: " ++ String.fromInt int }

            _ -> 
              { model | error = Just "Ran into a thing, it hurt a lot. Can you tell us about it?" }

    PushedButton ->
      case model.title of 
        Just "" -> 
         { model | title = Nothing }
 
        _ -> 
         model

    SetTitle next ->
      { model | title = Just next }

    SetSpeed next ->
      { model | speed = next }

    ToggleVoice voice ->
      { model | voices = Tools.toggleElement voice model.voices }    


titleBox : (Maybe String) -> (String ->msg) -> Html msg
titleBox current change = 
  div [ Attr.class "is-flex is-justify-content-center"]
    [ p [] [ text "What is the name of this song?" ]
    ,  Components.textEditor "Rock A Way" (Maybe.withDefault "" current) change 
    ]


speedBox : Speed -> (Speed -> msg) -> Html msg
speedBox current change =
  let 
    show = (\s -> 
      if s == current then 
        [ Attr.style "z-index" "2"
        , Attr.class "selected has-background-success" ]
      else [])
  in 
  div [Attr.class "my-6"]
    [ p [Attr.class "my-3"] [ text "What tempo should this song be?" ]
    , Components.cols  <| List.map (\child -> Components.col [Attr.class "is-flex is-flex is-justify-content-center"] [child])
      [ Components.button (change Slow) (show Slow) "Slow"
      , Components.button (change Medium) (show Medium) "Medium"
      , Components.button (change Fast) (show Fast) "Fast"
      ] ]


voiceIconClass = Attr.class "is-flex is-flex-direction-column is-align-items-center column is-one-third is-one-third-mobile"
-- voiceIconClass = Attr.class "column is-3"


goButton : String -> msg -> Html msg
goButton title msg = 
  if "" == title then 
    Components.button msg [ Attr.class "is-primary is-focused is-fullwidth" ] "Write a Song"
  else 
    Components.button msg [ Attr.class "is-primary is-focused is-fullwidth" ] ("Write \"" ++ title ++ "\"")


disabledGoButton : Html msg
disabledGoButton =
  Components.buttonDisabled [  Attr.class "is-primary is-fullwidth" ] "Write a Song"


fireButton : Model -> msg ->  Html msg
fireButton state msg =
  if List.length state.voices == 0 then
    div [ ]
      [ disabledGoButton 
      , p [ Attr.class "my-3 has-text-danger" ] [ text "Add at least one voice to write a song." ]
      ] 
  else 
    case state.title of
       Nothing ->
         div [ ]
           [ disabledGoButton 
           , p [ Attr.class "my-3 has-text-danger" ] [ text "Your song needs a name, can you give it one before we write it?" ]
           ]  
       Just title -> 
         goButton title msg


disabledIcon : SynthRole -> Html msg
disabledIcon role =
  div [ voiceIconClass
      , Attr.style "filter" "blur(0.15rem) opacity(50%)" ] 
      [ Components.box <| List.singleton <| View.roleIcon role
      , p [] [text <| Data.roleName role ] ]


selectedIcon role toggle =
    div [ voiceIconClass ] 
      [ div [toggle] [View.roleIconColored role]
      , p [toggle] [text <| Data.roleName role ] ]


availableIcon role toggle =
    div [voiceIconClass ] 
      [ Components.box <| List.singleton <| div [toggle] [View.roleIcon role]
      , p [toggle] [text <| Data.roleName role ] ]


voiceBox : (List SynthRole) -> (SynthRole -> msg) -> Html msg
voiceBox current change =
 div [ Attr.class "my-6"]
    [ p [Attr.class "mt-3"] [ text "Which voices should we put in it?" ]
    , p [Attr.class "mb-3"] [ text "Pick up to 4 voices." ]
    , div [ Attr.class "my-3 columns is-multiline is-mobile is-tablet is-desktop" ]  <| List.map (\r -> 
       if List.member r current then 
         selectedIcon r (onClick <| change r) 
       else if 4 > List.length current then
         availableIcon r (onClick <| change r) 
       else
         disabledIcon r )  synthRoles 
    ]


description : Html msg
description =
  div []
    [ p [] [ text "Hi! I'm your Mini Song Maker." ]
    , p [] [ text "Use me to write a 15 second song right now." ]
    ] 


view : Model -> Html Msg
view state =
  Components.box
    [ Components.heading "Mini Song Maker"
    , Components.cols
        [ Components.col1 description
        , Components.col1 <| fireButton state (if validModel state then ReqTrack else PushedButton)
        ] 
    , titleBox state.title SetTitle
    , speedBox state.speed SetSpeed
    , voiceBox state.voices ToggleVoice
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (apply msg model, Cmd.none)


main =  element 
  { init = init
  , update = update
  , view = view
  , subscriptions = (\_ -> Sub.none)
  }
