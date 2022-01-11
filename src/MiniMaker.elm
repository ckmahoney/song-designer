module MiniMaker exposing (Model, view)
-- Instance of a song maker with limited options

import Browser exposing (element)
import Html exposing (Html, div, p, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)

import Types exposing (SynthRole(..))
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
  }


type Msg 
  = SetTitle String
  | SetSpeed Speed
  | ToggleVoice SynthRole
  | PushedButton

initModel : Model
initModel =
  { title = Just ""
  , speed = Medium
  , voices = [ Kick, Hat, Melody ]
  }


init : Maybe Int -> (Model, Cmd Msg)
init _ =
  (initModel, Cmd.none)
  

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
        , Components.col1 <| fireButton state PushedButton
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
