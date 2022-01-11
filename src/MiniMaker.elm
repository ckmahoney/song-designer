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
  { title = Just "Rock A Way"
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
  div [Attr.class "is-flex is-justify-content-center"]
    [  Components.textEditor "My Amazing Song" (Maybe.withDefault "" current) change ]


speedBox : Speed -> (Speed -> msg) -> Html msg
speedBox current change =
  let 
    show = (\s -> 
      if s == current then 
        [ Attr.style "z-index" "2"
        , Attr.class "selected has-background-success" ]
      else [])
  in 
  Components.colsWith [Attr.class "my-3"]  <| List.map (\child -> Components.col [Attr.class "is-flex is-flex is-justify-content-center"] [child])
    [ Components.button (change Slow) (show Slow) "Slow"
    , Components.button (change Medium) (show Medium) "Medium"
    , Components.button (change Fast) (show Fast) "Fast"
    ]


voiceIconClass = Attr.class "is-flex is-flex-direction-column is-align-items-center column is-one-third"


goButton : msg -> Html msg
goButton msg = 
  Components.button msg [ Attr.class "is-primary is-focused is-fullwidth" ] "Write a Song"


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
  else if state.title == Nothing then 
    div [ ]
      [ disabledGoButton 
      , p [ Attr.class "my-3 has-text-danger" ] [ text "Give your song a name! It can be anything. Really." ]
      ]  
  else 
    goButton msg


disabledIcon : SynthRole -> Html msg
disabledIcon role =
  div [ voiceIconClass
      , Attr.style "filter" "blur(0.15rem) opacity(50%)" ] 
      [ Components.box <| List.singleton <| View.roleIcon role
      , p [] [text <| Data.roleName role ] ]


selectedIcon role toggle =
    div [ voiceIconClass ] 
      [ div [toggle] [View.roleIconColored role]
      , p [] [text <| Data.roleName role ] ]


availableIcon role toggle =
    div [voiceIconClass ] 
      [ Components.box <| List.singleton <| div [toggle] [View.roleIcon role]
      , p [] [text <| Data.roleName role ] ]


voiceBox : (List SynthRole) -> (SynthRole -> msg) -> Html msg
voiceBox current change =
 Components.colsWith [Attr.class "is-multiline my-3"] 
    <| List.map (\r -> 
       if List.member r current then 
         selectedIcon r (onClick <| change r) 
       else if 4 > List.length current then
         availableIcon r (onClick <| change r) 
       else
         disabledIcon r )  synthRoles


description : Html msg
description =
  div []
    [ p [] [ text "Hi! I'm your Mini Song Maker. Use me to write a 15 second song right now." ]
    , p [] [ text "What is the name of this song?" ]
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
    , p [] [ text "What tempo should this song be?" ]
    , speedBox state.speed SetSpeed
    , p [] [ text "Which voices should we put in it?" ]
    , p [] [ text "Pick up to 4 voices." ]
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
