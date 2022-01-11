module MiniMaker exposing (Model, view)
-- Instance of a song maker with limited options

import Browser exposing (element)
import Html exposing (Html, div, p, text)
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
  { title = "Rock a way"
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

speedBox =
  div []
    [ Components.button (SetSpeed Slow) [] "Slow"
    , Components.button (SetSpeed Medium) [] "Medium"
    , Components.button (SetSpeed Fast) [] "Fast"
    ]


voiceBox : (List SynthRole) -> (SynthRole -> msg) -> Html msg
voiceBox current change =
 let
  child = (\(r, c) -> div [onClick <|  change r] [ c ])
 in 
  div []
    <| List.map (\r -> 
       if List.member r current then 
         child <| (r, View.roleIconColored r)
       else 
         child <| (r, View.roleIcon r))  synthRoles


view : Model -> Html Msg
view state =
  div []
    [ Components.heading "Mini Song Maker"
    , p [] [ text "Hi! I'm your Mini Song Maker. Use me to write a 15 second song right now." ]
    , p [] [ text "What is the name of this song?" ]
    , Components.textEditor "My Amazing Song" state.title SetTitle
    , p [] [ text "What tempo should this song be?" ]
    , speedBox
    , p [] [ text "Which voices should we put in it?" ]
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
