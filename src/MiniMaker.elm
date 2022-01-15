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


type alias PendingMember =
  { name : String
  , email : String
  , tracks : List Int
  } 


type alias Model =
  { member : GhostMember
  , pending : Maybe PendingMember
  , pendingError : Maybe String
  , pendingSubmitted : Maybe String
  , title : Maybe String
  , speed : Speed
  , voices: List SynthRole
  , tracks : List TrackMeta
  , error : Maybe String
  , status : Maybe String
  , player : Playback.Player
  }


-- current probem: 
-- when I try to change the track, it does not look like it chagnes. 


encodeReqTrack : String -> String -> String -> Combo -> Encode.Value
encodeReqTrack email uuid title ((scope,ensemble) as combo) =
  Encode.object
    [ ("meta", JE.encodeScoreMeta <| makeMeta title scope)
    , ("layout", JE.encodeLayout [combo])
    , ("email", Encode.string email)
    , ("uuid", Encode.string uuid)
    ]


type Msg 
  = SetTitle String
  | SetSpeed Speed
  | ToggleVoice SynthRole
  | PushedButton
  | UpdateName String
  | UpdateEmail String
  | ClickedRegister

  -- Random generators
  | RollForTrack
  | RolledCombo Combo

  -- Sidefx things 
  | UpdatePlayer (Playback.Player, Playback.Msg)
  | Download String
  | GotTrack (Result Http.Error TrackMeta)
  | CompletedReg (Result Http.Error String)
  | RegisterUser PendingMember


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


reqRegister : PendingMember -> Cmd Msg
reqRegister {email, name} =
  Http.post
    { url = Conf.regUrl
    , body = Http.jsonBody <| JE.encodeReqRegister <| Conf.regData email name
    , expect = Http.expectString CompletedReg
    }


makeMeta : String -> Scope -> ScoreMeta
makeMeta title scope =
  ScoreMeta title scope.cps (toFloat scope.root) scope.cpc


initModel : Model
initModel =
  { member = Conf.anonMember
  , pending = Nothing
  , pendingError = Nothing
  , pendingSubmitted = Nothing
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
    Nothing -> -- use the anon member and open a  pending member
      ({ initModel | pending = Just { name = "", email = "", tracks = [] } }, Cmd.none)
    
    Just member ->
      ({ initModel | member =  member }, Cmd.none)


validReq : Model -> Bool
validReq state = 
  case state.title of 
    Nothing -> False
    Just "" -> False
    Just title -> 
      List.length state.voices > 0 


updatePending : PendingMember -> String -> String -> PendingMember
updatePending p field val  =
  if field == "name" then 
    { p | name = val }
  else if field == "email" then 
    { p | email = val }
  else 
    { name = "", email = "", tracks = [] }


apply : Msg -> Model -> Model
apply msg model =
  case msg of       
    UpdateName name -> 
     case model.pending of 
       Nothing -> { model | pending = Just { name = name, email = "", tracks = [] } }
       Just p -> { model | pending = Just <| updatePending p "name" name }

    UpdateEmail email -> 
     case model.pending of 
       Nothing -> { model | pending = Just { name = "", email = email, tracks = [] } }
       Just p -> { model | pending = Just <| updatePending p "email" email }


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
  div [ Attr.class "is-flex is-justify-content-center columns"]
    [ Components.col1 <| p [] [ text "What is the name of this song?" ]
    , Components.col1 <| Components.textEditor "Rock A Way" (Maybe.withDefault "" current) change 
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


-- Controls for the MiniMaker 
makerBoxes : Model -> (Html Msg) -> Html Msg
makerBoxes state button = 
  case (state.status, state.error) of 
    (Nothing, Nothing) ->
      div []
        [ titleBox state.title SetTitle
        , speedBox state.speed SetSpeed
        , voiceBox state.voices ToggleVoice
        , button
        ]

    _ ->
     text ""


pendingErrMessage : PendingMember -> Maybe String
pendingErrMessage {name, email} =
  if String.length name < 3 then 
    Just "That name is too short, can you make it longer?"
  else if True /= String.contains "@" email then 
    Just "We need an email address to log you in."
  else if String.length email < 6 then 
    Just "That email address is funky, can you fix it?"
  else
    Nothing


pendingIsOK : PendingMember -> Bool
pendingIsOK pending =
  if Nothing == pendingErrMessage pending then True else False


cta : PendingMember -> (String -> msg) -> (String -> msg) -> msg -> (Maybe String) -> Html msg
cta pending uName uEmail register maybeError =
  Components.box <| 
    [ p [Attr.class "mb-3"] [text "Music is a very fleeting thing. These short songs will disappear into the void..."]
    , p [Attr.class "mb-3"] [text "But you can save and keep them if you want. Just sign up here, it's easy and free."]
    , Components.label "Name" 
    , Components.textEditor "Name" pending.name uName
    , Components.label "Email" 
    , Components.textEditor "Email" pending.email uEmail
    , Components.button register [Attr.class "button is-primary"]  "Join Synthony!"
    , case maybeError of 
        Nothing -> text ""
        Just msg -> p [Attr.class "has-text-danger"] [text msg]
    ]


showCta : Model -> PendingMember -> Msg -> Html Msg
showCta state pending register =
  if state.member /= Conf.anonMember then 
    text ""

  else if List.length state.tracks > 2 then 
    case state.pendingSubmitted of 
      Just submitted -> 
        Components.box <| List.singleton <| Components.cols <|
          [ Components.col1 <| text submitted
          , Components.col1 <| Components.button (RegisterUser pending) [Attr.class "button is-warning"] "Try Again"
          ]

      Nothing -> 
        cta pending UpdateName UpdateEmail (if pendingIsOK pending then RegisterUser pending else ClickedRegister) state.pendingError

  else text ""


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
        ] 
    , case state.pending of 
        Nothing -> text ""
        Just p -> showCta state p (RegisterUser p)
    , Playback.mini state.player UpdatePlayer state.tracks Download 
    , postBox state
    , makerBoxes state butt
    ]


-- intercepts Cmd msgs, otherwise passes pur updates to apply
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of  
    CompletedReg response ->
      case response of 
        Ok message ->
          ( { model | pending = Nothing
            , pendingSubmitted = Just "Amazing fam. We sent you an email, make sure you open it" }, Cmd.none)

        Err errr ->
          case errr of 
            Http.BadBody str -> 
              ({ model | status = Nothing
               , pendingSubmitted = Just "Did you try to break something or was that us?"  }, Cmd.none)
 
            Http.BadUrl str -> 
              ({ model | status = Nothing
               , pendingSubmitted = Just "Where did you get that URL?" }, Cmd.none)

            Http.BadStatus int -> 
              ({ model | status = Nothing
               , pendingSubmitted = Just <| "The server looks like it had a problem. Here's a hint: " ++ String.fromInt int }, Cmd.none)

            _ -> 
              ({ model | status = Nothing, error = Just "Ran into a thing, it hurt a lot. Can you tell us about it?" }, Cmd.none)

    ClickedRegister ->
     case model.pending of 
        Nothing -> (model, Cmd.none) -- weird how did you get here
        Just pending -> 
         let
           message = pendingErrMessage pending
         in
         case message of 
          Nothing ->
           update (RegisterUser pending) model

          Just error ->
           ({ model | pendingError = Just error }, Cmd.none )

    RegisterUser pending ->
          (model, reqRegister pending)

    Download url ->
      (model, Conf.download url)

    RollForTrack ->
      ({ model | status = Just "Rolling some dice..." }, modelToCombo (Maybe.withDefault "" model.title) model.speed model.voices)

    RolledCombo combo ->
      ({ model | status = Just "Writing a track for you!"}, reqTrack model.member.email model.member.uuid (Maybe.withDefault "" model.title) combo)

    GotTrack response ->
      case response of 
        Ok track ->
         let
            newTracks = track :: model.tracks 
            pending = case model.pending of 
              Just p -> 
                Just { p | tracks = List.map .id newTracks }
              _ -> Nothing
         in 
          ({ model | status = Nothing
          , tracks = newTracks
          , pending = pending
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

    UpdatePlayer ((mTrack, playState) as player, pMsg) ->
      case mTrack of
        Nothing ->
         ({ model | player = Playback.new}, Playback.stopMusic "")

        Just t ->
           case pMsg  of 
            Playback.Load (nodeId, path) -> 
              ( { model | player = player }
              , Playback.trigger <| Playback.Load ("#the-player", Conf.hostname ++ path))

            Playback.Select (Just track) ->    
              ( { model | player = player }
              , Playback.trigger <| Playback.Select (Just track)) -- fixes the missing hostname on getSongs

            _ ->
             ( { model | player = player }
             , Playback.trigger pMsg )


    _ -> 
      (apply msg model, Cmd.none)


main =  element 
  { init = init
  , update = update
  , view = view
  , subscriptions = (\_ -> Sub.none)
  }
