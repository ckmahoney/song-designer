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
  { title : String
  , speed : Speed
  , voices: List SynthRole
  }


type Msg 
  = SetTitle String
  | SetSpeed Speed
  | ToggleVoice SynthRole


initModel : Model
initModel =
  { title = "Rock A Way"
  , speed = Medium
  , voices = [ Kick, Hat, Melody ]
  }


init : Maybe Int -> (Model, Cmd Msg)
init _ =
  (initModel, Cmd.none)
  

apply : Msg -> Model -> Model
apply msg model =
  case msg of 
    SetTitle next ->
      { model | title = next }

    SetSpeed next ->
      { model | speed = next }

    ToggleVoice voice ->
      { model | voices = Tools.toggleElement voice model.voices }    



titleBox : String -> (String ->msg) -> Html msg
titleBox current change = 
  div [Attr.class "is-flex is-justify-content-center"]
    [  Components.textEditor "My Amazing Song" current change ]


speedBox : Speed -> (Speed -> msg) -> Html msg
speedBox current change =
  let 
    highlight = (\s -> 
      if s == current then 
        [Attr.style "z-index" "2", Attr.class "selected has-background-success" ]
      else [])
  in 
  Components.colsWith [Attr.class "my-3"]  <| List.map (\child -> Components.col [Attr.class "is-flex is-flex is-justify-content-center"] [child])
    [ Components.button (change Slow) (highlight Slow) "Slow"
    , Components.button (change Medium) (highlight Medium) "Medium"
    , Components.button (change Fast) (highlight Fast) "Fast"
    ]



voiceIconClass = Attr.class "is-flex is-flex-direction-column is-align-items-center column is-one-third"


disabledIcon : SynthRole -> Html msg
disabledIcon role =
  div [ voiceIconClass
      , Attr.style "filter" "blur(0.15rem) opacity(50%)" ] 
      [ Components.box <| List.singleton <| View.roleIcon role
      , p [] [text <| Data.roleName role ] ]


selectedIcon role toggle =
    div [ voiceIconClass, toggle ] 
      [ View.roleIconColored role
      , p [] [text <| Data.roleName role ] ]


availableIcon role toggle =
    div [voiceIconClass, toggle ] 
      [ Components.box <| List.singleton <| View.roleIcon role
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
         disabledIcon r  )  synthRoles


view : Model -> Html Msg
view state =
  Components.box
    [ Components.heading "Mini Song Maker"
    , p [] [ text "Hi! I'm your Mini Song Maker. Use me to write a 15 second song right now." ]
    , p [] [ text "What is the name of this song?" ]
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
