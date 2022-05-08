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

import Defs.Types exposing (GhostMember, SynthRole(..), ScoreMeta, TrackMeta, Template, Layout, Combo, SynthRole, Scope, Ensemble, Voice)
import Defs.Data exposing (synthRoles)
import Defs.Configs as Conf
import Components
import Comm.Playback
import Components.View as View
import Tools
import Components.Embeds as Embeds
import Comm.Ports exposing (scrollTo, setStorage)



scroll : String -> Cmd  Msg
scroll id = 
  scrollTo id

type Speed 
  = Slow
  | Medium 
  | Fast


type alias PendingMember =
  { name : String
  , email : String
  , trackIDs : List Int
  } 


type alias Model =
  { member : GhostMember
  , pendingMember : Maybe PendingMember
  , pendingMemberError : Maybe String
  , pendingMemberSubmitted : Maybe String
  , title : Maybe String
  , speed : Speed
  , voices: List SynthRole
  , tracks : List TrackMeta
  , error : Maybe String
  , status : Maybe String
  , playback : Playback.Model
  , helpTexts : List SynthRole
  , showSample : Maybe SynthRole
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
  | ToggleHelp SynthRole
  | PushedButton
  | UpdateName String
  | UpdateEmail String
  | ClickedRegister
  | PlaySample SynthRole
  | CloseSample

  -- Random generators
  | RollForTrack
  | RolledCombo Combo

  -- Sidefx things 
  | UpdatePlayer (Playback.Model, Playback.Msg)
  | Download String
  | GotTrack (Result Http.Error TrackMeta)
  | CompletedReg (Result Http.Error String)
  | RegisterUser PendingMember

  | Scroll String
  | Noop (Result Http.Error String)


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
reqRegister {email, name, trackIDs} =
  Http.post
    { url = Conf.selfUrl Conf.regUrl
    , body = Http.jsonBody <| JE.encodeReqRegister <| Conf.regData email name trackIDs
    , expect = Http.expectString CompletedReg
    }


reqLead : PendingMember -> Cmd Msg
reqLead {email, name, trackIDs} =
  Http.get
    { url = Conf.apiUrl <| JE.queryReqLead email name trackIDs "minimaker"
    , expect = Http.expectString Noop
    }


makeMeta : String -> Scope -> ScoreMeta
makeMeta title scope =
  ScoreMeta title scope.cps (toFloat scope.root) scope.cpc


initModel : Model
initModel =
  { member = Conf.anonMember
  , pendingMember = Nothing
  , pendingMemberError = Nothing
  , pendingMemberSubmitted = Nothing
  , title = Just ""
  , speed = Medium
  , voices = []
  , tracks = []
  , error = Nothing
  , status = Nothing
  , playback = Playback.new
  , helpTexts = []
  , showSample = Nothing
  }


init : Maybe GhostMember -> (Model, Cmd Msg)
init flags =
  case flags of 
    Nothing -> -- use the anon member and open a pending member
      ({ initModel | pendingMember = Just { name = "", email = "", trackIDs = [] } }, Cmd.none)
    
    Just member ->
      ({ initModel | member =  member }, Cmd.none)


-- Checks that the config made in the Mini Maker has the minimum requirements for a song.
validReq : Model -> Bool
validReq state = 
  case state.title of 
    Nothing -> False
    Just "" -> False
    Just title -> 
      List.length state.voices > 0 


updatePendingMember : PendingMember -> String -> String -> PendingMember
updatePendingMember p field val  =
  if field == "name" then 
    { p | name = val }
  else if field == "email" then 
    { p | email = val }
  else 
    { name = "", email = "", trackIDs = [] }


apply : Msg -> Model -> Model
apply msg model =
  case msg of       
    UpdateName name -> 
     case model.pendingMember of 
       Nothing -> { model | pendingMember = Just { name = name, email = "", trackIDs = [] } }
       Just p -> { model | pendingMember = Just <| updatePendingMember p "name" name }

    UpdateEmail email -> 
     case model.pendingMember of 
       Nothing -> { model | pendingMember = Just { name = "", email = email, trackIDs = [] } }
       Just p -> { model | pendingMember = Just <| updatePendingMember p "email" email }

    PushedButton ->
      if List.length model.voices == 0 then 
        { model | error = Just "Click on a voice to add it to your song." } 

      else case model.title of 
        Just "" -> 
         { model | error = Just "Your song needs a name, can you give it one before we write it?" }
 
        Nothing -> 
         { model | error = Just "Your song needs a name, can you give it one before we write it?" }
 
        _ -> 
         { model | error = Nothing }

    SetTitle next ->
      { model | title = Just next, error = Nothing }

    SetSpeed next ->
      { model | speed = next }

    ToggleVoice voice ->
      { model 
      | voices = Tools.toggleElement voice model.voices
      , error = Nothing }    

    ToggleHelp voice ->
      { model | helpTexts = Tools.toggleElement voice model.helpTexts }    

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
    Components.button msg [ Attr.class "is-success is-focused is-fullwidth" ] "Make a Song"
  else 
    Components.button msg [ Attr.class "is-success is-focused is-fullwidth" ] ("Make \"" ++ title ++ "\"")


disabledGoButton : Html msg
disabledGoButton =
  Components.buttonDisabled [  Attr.class "is-success is-fullwidth" ] "Make a Song"


unfoundBugAsk : Html msg
unfoundBugAsk =
  Html.a [Attr.href "/contact"] [ text "Super weird bug! We didn't think this could happen. Can you tell us about it?" ]


fireButton : Model -> msg ->  Html msg
fireButton state msg =
  case state.error of 
    Just message ->
      div []
        [ disabledGoButton 
        , p [ Attr.class "my-3 has-text-danger" ] [ text message ]
        ]
    Nothing ->
      case state.title of 
        Just title -> 
         goButton title msg
        Nothing ->
         unfoundBugAsk


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


availableIconOld role toggle =
    div [voiceIconClass ] 
      [ Components.box <| List.singleton <| div [toggle] [View.roleIcon role]
      , p [toggle] [text <| Data.roleName role ] ]


availableIcon : SynthRole -> (SynthRole -> msg) -> Bool -> (SynthRole -> msg) -> msg -> Html msg
availableIcon role click showHelp toggleHelp toggleSample = 
   div [ voiceIconClass ]
     [ View.synthIconHelp role click showHelp toggleHelp toggleSample ]


voiceBox : (List SynthRole) -> (SynthRole -> msg) -> (List SynthRole) -> (SynthRole -> msg) -> (SynthRole -> msg) -> msg -> (Maybe SynthRole) -> Html msg
voiceBox current change helps showHelp playSample clearSample sample =
 div [ Attr.id "voice-box", Attr.class " mb-6"]
    [ div [Attr.class "content"]
      [ p [Attr.class "mt-3"] [ text "Which voices should we put in it? Pick up to 4." ] 
      , p [Attr.class "is-size-4 is-block has-text-centered"] [ text "Not sure which voices to pick? Read the ", Html.a [ Attr.href "/articles/quickstart", Attr.target "_blank" ] [ text "Quickstart" ], text " for some tips!" ]
      ]
    , div [ Attr.class "mb-3 columns is-multiline is-mobile is-tablet is-desktop" ]  <| List.map (\r -> 
       if List.member r current then 
         selectedIcon r (onClick <| change r) 
       else if 4 > List.length current then
        let
         toggleSample = case sample of 
           Nothing -> playSample r
           Just samp -> if samp == r then clearSample else playSample r
        in 
         availableIcon r change (List.member r helps) showHelp toggleSample
       else
         disabledIcon r )  Data.synthRoles
    , p [Attr.class "is-size-4 is-block has-text-centered"] [ text "Want more voices? Use the ", Html.a [Attr.style "text-decoration" "underline", Attr.href "/song-designer"] [text "Song Designer"], text " for unlimited voices and sections." ]
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
      div [Attr.class "is-warning is-light"]
        [ p [] [text message] ]


postBox : String -> Html msg
postBox status =
  Components.box 
  [ div [Attr.class "notification is-success is-light"]
    [ p [] [ text status] ]
  ]


-- Controls for the MiniMaker 
makerBoxes : Model -> (Html Msg) -> Html Msg
makerBoxes state button = 
  let
    class = case (state.status) of 
      Nothing -> ""
      _ -> "overlay-disabled"

  in 
  div []
    [ div [Attr.class class]
      [ titleBox state.title SetTitle
      , speedBox state.speed SetSpeed
      , voiceBox state.voices ToggleVoice state.helpTexts ToggleHelp PlaySample CloseSample state.showSample
      ]
      , button 
    ]


pendingMemberErrMessage : PendingMember -> Maybe String
pendingMemberErrMessage {name, email} =
  if String.length name < 3 then 
    Just "That name is too short, can you make it longer?"
  else if True /= String.contains "@" email then 
    Just "We need an email address to log you in."
  else if String.length email < 6 then 
    Just "That email address is funky, can you fix it?"
  else
    Nothing


pendingMemberIsOK : PendingMember -> Bool
pendingMemberIsOK pendingMember = 
 if Nothing == pendingMemberErrMessage pendingMember then True else False


cta : PendingMember -> (String -> msg) -> (String -> msg) -> msg -> (Maybe String) -> Html msg
cta pendingMember uName uEmail register maybeError =
  Components.box <| 
    [ p [Attr.class "mb-3"] [text "Music is a very fleeting thing. These short songs will disappear into the void..."]
    , p [Attr.class "mb-3"] [text "To keep your songs and download them anywhere, just join here :)"]
    , Components.label "Name" 
    , Components.textEditor "Name" pendingMember.name uName
    , Components.label "Email" 
    , Components.textEditor "Email" pendingMember.email uEmail
    , Components.button register [Attr.class "button is-success"]  "Join Synthony!"
    , case maybeError of 
        Nothing -> text ""
        Just msg -> p [Attr.class "has-text-danger"] [text msg]
    ]


showCta : Model -> PendingMember -> Msg -> Html Msg
showCta state pendingMember register =
  if state.member /= Conf.anonMember then 
    text ""

  else if List.length state.tracks > 0 then 
    case state.pendingMemberSubmitted of 
      Just submitted -> 
        Components.box <| List.singleton <| Components.cols <|
          [ Components.col1 <| text submitted
          , Components.col1 <| Components.button (RegisterUser pendingMember) [Attr.class "button is-warning"] "Try Again"
          ]

      Nothing -> 
        cta pendingMember UpdateName UpdateEmail (if pendingMemberIsOK pendingMember then RegisterUser pendingMember else ClickedRegister) state.pendingMemberError

  else text ""


sampleBox : SynthRole -> msg -> Html msg
sampleBox role close =
  div [ Attr.id "sample-box" ] 
  [ Components.colsWith [ Attr.class "is-justify-content-space-between" ]
    [ Components.col1 <| Components.paragraph <| "This sample demonstrates what " ++ Data.roleName role ++ " sounds like."
    , Components.col1 <| div [ Attr.class "is-flex is-justify-content-flex-end is-align-items-center"] 
        [ Components.button close [ Attr.class "is-success is-focused" ] "Close Sample to Make a Song"
        ]
    ]
  , div [ Attr.class "slide-in-out"
        -- , Attr.style "margin-top" <| String.fromInt <| if Tools.isNothing mRole then Embeds.scEmbedHeight else 0 
        -- , Attr.class <| if Tools.isNothing mRole then "hidden" else "visible" 
        ]
      [ Embeds.soundcloud role ]
  ]
  

view : Model -> Html Msg
view model =
 let 
  butt = case model.status of 
    Nothing ->
      Components.col1 <| case model.showSample of 
        Nothing -> 
         fireButton model (if validReq model then RollForTrack else PushedButton )
        Just role -> 
         sampleBox role CloseSample
    Just status ->
      postBox status
 in 
  Components.boxWith "mb-6"
    [ Components.heading "Mini Song Maker"
    , Components.cols
        [ Components.col1 description ] 
    , case model.pendingMember of 
        Nothing -> text ""
        Just p -> showCta model p (RegisterUser p)
    , Playback.mini model.playback UpdatePlayer model.tracks Download 
    , makerBoxes model butt
    ]


-- intercepts Cmd msgs, otherwise passes pur updates to apply
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of  
    CloseSample -> 
        ( { model | showSample = Nothing }, scroll "#voice-box" ) 
      
    PlaySample role ->
      case Debug.log "sample" model.showSample of 
        Nothing -> 
           ( { model | showSample = Just role }, scroll "#sample-box" ) 

        Just prev -> 
          if role == prev then 
           ( { model | showSample = Nothing }, scroll "#voice-box" ) 
          else ( { model | showSample = Just role }, scroll "#sample-box" ) 

    CompletedReg response ->
      case response of 
        Ok message ->
          ( { model | pendingMember = Nothing
            , pendingMemberSubmitted = Just "Amazing fam. We sent you an email, make sure you open it to join the future of music!" }, Cmd.none)

        Err errr ->
          case errr of 
            Http.BadBody str -> 
              ({ model | status = Nothing
               , pendingMemberSubmitted = Just "Did we break something or did you hax us?"  }, Cmd.none)
 
            Http.BadUrl str -> 
              ({ model | status = Nothing
               , pendingMemberSubmitted = Just "That request URL looks really strange to us." }, Cmd.none)

            Http.BadStatus int -> 
              ({ model | status = Nothing
               , pendingMemberSubmitted = Just <| "The server looks like it had a problem. Here's a hint: " ++ String.fromInt int }, Cmd.none)

            _ -> 
              ({ model | status = Nothing, error = Just "Ran into a thing, it hurt a lot. Can you tell us about it?" }, Cmd.none)

    ClickedRegister ->
     case model.pendingMember of 
        Nothing -> (model, Cmd.none) -- weird how did you get here
        Just pendingMember -> 
         let
           message = pendingMemberErrMessage pendingMember
         in
         case message of 
          Nothing ->
           update (RegisterUser pendingMember) model

          Just error ->
           ({ model | pendingMemberError = Just error }, Cmd.none )

    RegisterUser pendingMember ->
          ( model
          , Cmd.batch 
            [ reqRegister pendingMember
            , reqLead pendingMember
            ]
          )

    Download url ->
      (model, Conf.download url)

    RollForTrack ->
      ( { model | status = Just "Rolling some dice..." }
      , modelToCombo (Maybe.withDefault "" model.title) model.speed model.voices
      )

    RolledCombo combo ->
      ( { model | status = Just "Making a track for you!" }
      , reqTrack model.member.email model.member.uuid (Maybe.withDefault "" model.title) combo
      )

    UpdatePlayer (playback, pMsg) ->
      let
        ((updated, pMsg_) as result) = Playback.update pMsg playback
        next = { model | playback = updated }
      in 
              
      case Tuple.first updated of
        Nothing ->
         ( next, Playback.trigger pMsg_ )

        Just track ->
         ( next, Playback.trigger pMsg_ )


    _ -> 
      (apply msg model, Cmd.none)


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
  case msg of 
    GotTrack response ->
      case response of 
        Ok track ->
         let
            newTracks = track :: model.tracks 
            pendingMember = case model.pendingMember of 
              Just p ->  -- keep new track IDs to pass to new user registrations
                Just { p | trackIDs = List.map .id newTracks }
              _ -> Nothing
         in 
          ({ model 
          | status = Nothing
          , tracks = newTracks
          , pendingMember = pendingMember
          , playback = (Just track, Playback.Playing)
          }, Cmd.batch 
              [ Playback.trigger <| Playback.Load <|  Conf.hostname ++ track.filepath
              , scroll "#minimaker"
              , setStorage ("trackIDs", (JE.ints <| List.map .id newTracks))
              ]
          )

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
    _ -> 
      update msg model



main =  element 
  { init = init
  , update = updateWithStorage
  , view = view
  , subscriptions = (\_ -> Sub.none)
  }
