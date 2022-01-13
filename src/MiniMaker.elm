module MiniMaker exposing (Model, view)
-- Instance of a song maker with limited options

import Browser exposing (element)
import Html exposing (Html, div, p, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Random
import Random.Extra
import Encoders as JE
import Decoders as JD
import Json.Decode as Decode
import Json.Encode as Encode


import Types exposing (GhostMember, SynthRole(..), ScoreMeta, TrackMeta, Template, Layout, Combo, SynthRole, Scope, Ensemble, Voice)
import Data exposing (synthRoles)
import Configs as Conf
import Components
import Playback
import View
import Tools


type Speed 
  = Slow
  | Medium 
  | Fast


type alias Model =
  { member : GhostMember
  , title : Maybe String
  , speed : Speed
  , voices: List SynthRole
  , tracks : List TrackMeta
  , error : Maybe String
  , status : Maybe String
  , player : Playback.Player
  }



encodeReqTrack : String -> String -> String -> Combo -> Encode.Value
encodeReqTrack email uuid title ((scope,ensemble) as combo) =
  Encode.object
    [ ("meta", JE.encodeScoreMeta <| makeMeta title scope)
    , ("layout", JE.encodeLayout [combo])
    , ("email", Encode.string email)
    , ("uuid", Encode.string uuid)
    ]


type Msg 
  = UpdatePlayer (Playback.Player, Playback.Msg)
  | SetTitle String
  | SetSpeed Speed
  | ToggleVoice SynthRole
  | PushedButton

  | RollForTrack
  | RolledCombo Combo

  | GotTrack (Result Http.Error TrackMeta)
  | Passthrough -- do nothing
  | Download String
 

miniMakerMember =
  { uuid = ""
  , email = ""
  }


rollTexture : Random.Generator Int
rollTexture = 
  Random.int 1 3


rollCps : Speed -> Random.Generator Float
rollCps speed = 
  case speed of 
    Slow ->  Random.float 0.8 3
    Medium -> Random.float 4 8
    Fast -> Random.float 12 18


-- the Elm app right now sends requests using 0-11 semitone notation instead of Hz values
rollRoot : Random.Generator Int
rollRoot = 
  Random.int 0 11


rollCpc : Random.Generator Int
rollCpc = 
  Random.int 3 4


rollScope : String -> Speed -> Random.Generator Scope
rollScope title speed = 
 let
  id = 0
  size = case speed of 
    Slow -> 2
    Medium -> 4
    Fast -> 6
 in
  Random.map3 (\cps root cpc -> Scope id title cps cpc root size) (rollCps speed) rollRoot rollCpc


rollVoice : SynthRole -> Random.Generator Voice
rollVoice role =
  Random.map2 (Voice 0 Types.Structure role "label" (Data.voiceIndex role)) rollTexture rollTexture


rollEnsemble : List SynthRole -> Random.Generator Ensemble
rollEnsemble voices =
  Random.Extra.sequence (List.map rollVoice voices) 


rollComboOneVoice : String -> Speed -> SynthRole -> Random.Generator Combo
rollComboOneVoice title speed role =
  Random.map2 (\meta voice -> (meta, [voice]))
    (rollScope title speed) 
    (rollVoice role)


rollCombo : String -> Speed -> List SynthRole -> Random.Generator Combo
rollCombo title speed roles =
  Random.pair
    (rollScope title speed) 
    (rollEnsemble roles)


modelToCombo : String -> Speed -> (List SynthRole) -> Cmd Msg
modelToCombo title speed roles =
 Random.generate RolledCombo (rollCombo title speed roles)
  

reqTrack : String -> String -> String -> Combo -> Cmd Msg
reqTrack email uuid title combo =
  Http.post
    { url = Conf.apiUrl "track"
    , body =  Http.jsonBody <| encodeReqTrack email uuid title combo
    , expect = Http.expectJson GotTrack JD.decodeTrack
    }


makeMeta : String -> Scope -> ScoreMeta
makeMeta title scope =
  ScoreMeta title scope.cps (toFloat scope.root) scope.cpc


initModel : Model
initModel =
  { member = Data.anonMember
  , title = Just ""
  , speed = Medium
  , voices = [ Kick, Hat, Melody ]
  , tracks = []
  , error = Nothing
  , status = Nothing
  , player = Playback.new
  }


init : Maybe GhostMember -> (Model, Cmd Msg)
init flags =
  case flags of 
    Nothing -> 
      (initModel, Cmd.none)
    
    Just member ->
      ({ initModel | member =  member }, Cmd.none)



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

    _ ->
      model


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
    , p [] [ text "Use me to write a short song right now." ]
    ] 


errorBox : (Maybe String) -> Html msg
errorBox str =
  case str of 
    Nothing -> text ""
    Just message ->
      p [] [text message]


statusBox : (Maybe String) -> Html msg
statusBox str =
  case str of 
    Nothing -> text ""
    Just message ->
      p [] [text message]


postBox : Model -> Html msg
postBox state =
  div [] 
    [ errorBox state.error
    , statusBox state.status
    ]


boxes : Model -> Html Msg
boxes state = 
  case (state.status, state.error) of 
    (Nothing, Nothing) ->
      div []
        [ titleBox state.title SetTitle
        , speedBox state.speed SetSpeed
        , voiceBox state.voices ToggleVoice
        ]

    _ ->
     text ""


view : Model -> Html Msg
view state =
 let 
  cb = (if validReq state then RollForTrack else PushedButton)
  butt =   case (state.status, state.error) of 
    (Nothing, Nothing) ->
      Components.col1 <| fireButton state cb
    _ ->
      text ""
 in 
  Components.box
    [ Components.heading "Mini Song Maker"
    , Components.cols
        [ Components.col1 description
        , butt
        ] 
    , Playback.mini state.player UpdatePlayer state.tracks Download 
    , postBox state
    , boxes state
    ]


-- intercepts Cmd msgs, otherwise passes pur updates to apply
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    Download url ->
      (model, Conf.download url)

    RollForTrack ->
      ({ model | status = Just "Rolling some dice..." }, modelToCombo (Maybe.withDefault "" model.title) model.speed model.voices)

    RolledCombo combo ->
      ({ model | status = Just "Writing a track for you!"}, reqTrack model.member.email model.member.uuid (Maybe.withDefault "" model.title) combo)

    GotTrack response ->
      case response of 
        Ok track ->
         ({ model | status = Nothing
         , tracks = track :: model.tracks 
         , player = (Just track, Playback.Playing)}, Playback.trigger <| Playback.Load ("#the-player", Conf.hostname ++ track.filepath))

        Err errr ->
          case errr of 
            Http.BadBody str -> 
              ({ model | status = Nothing, error = Just "How did you post that body?"  }, Cmd.none)
 
            Http.BadUrl str -> 
              ({ model | status = Nothing, error = Just "Where did you get that URL?" }, Cmd.none)

            Http.BadStatus int -> 
              ({ model | status = Nothing, error = Just <| "The server looks like it had a problem. Here's a hint: " ++ String.fromInt int }, Cmd.none)

            _ -> 
              ({ model | status = Nothing, error = Just "Ran into a thing, it hurt a lot. Can you tell us about it?" }, Cmd.none)

    UpdatePlayer ((mTrack, _) as playstate, pCmd) ->
      case mTrack of
        Nothing ->
         ({ model | player = Playback.new}, Playback.stopMusic "")

        Just t ->
           case pCmd of 
            Playback.Load (nodeId, path) -> 
              ( { model | player = playstate }, Playback.trigger <| Playback.Load ("#the-player", Conf.hostname ++ path))
            Playback.Select (Just track) ->    
              ( { model | player = playstate }, Playback.trigger <| Playback.Load ("#the-player", Conf.hostname ++ track.filepath)) -- fixes the missing hostname on getSongs

            _ ->
             ( { model | player = playstate }, Playback.trigger pCmd)


    _ -> 
      (apply msg model, Cmd.none)


main =  element 
  { init = init
  , update = update
  , view = view
  , subscriptions = (\_ -> Sub.none)
  }
