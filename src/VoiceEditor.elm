module VoiceEditor exposing (..)

import Browser
import Html exposing (Html, button, div, text, label, p, input)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)

import Types exposing (..)
import View
import Elements
import Tools
import Components
import Data


type alias State = Voice


type Msg
  = Save Model

type alias Model 
  = Voice



newVoice : Voice
newVoice =
  { id = -1
  , duty = Structure
  , role = Kick
  , label = ""
  , voice = 0
  , density = 1
  , complexity = 1
  } 



initModel : Model
initModel = 
  newVoice


updateLabel : Model -> (Model -> msg) -> String -> msg
updateLabel model update label =
  update { model | label = label }


updateRole : Model -> (Model -> msg) -> SynthRole -> msg
updateRole model update role =
  update { model | role = role }

updateDensity : Model -> (Model -> msg) -> Int -> msg
updateDensity model update density =
  update { model | density = density }


updateComplexity : Model -> (Model -> msg) -> Int -> msg
updateComplexity model update complexity =
  update { model | complexity = complexity }


edit : Model -> (Model -> msg) -> (Model -> msg) -> Html msg
edit model update save =
 let
   options = List.map (\r -> (r, View.roleIcon r)) Data.roles
 in 
  div [ Attr.class "container" ]
    [ Components.colsWith [ Attr.class "is-mobile is-flex is-justify-content-space-between" ] 
        [ Components.colHalf <| View.viewVoice model
        , Components.colHalf <| Components.editText "Label" (text "") model.label (updateLabel model update)
        ]  
    , Components.editSelection model.role "Role" (text "") options model.role (updateRole model update)
    , Components.colsWith [ Attr.class "is-mobile is-flex is-justify-content-space-between" ] 
       [ Components.colHalf <| Components.editInt "Density" (View.densityMessage model) Data.rangeDensity model.density (updateDensity model update)
       , Components.colHalf <| Components.editInt "Complexity" (View.complexityMessage model) Data.rangeComplexity model.complexity (updateComplexity model update) ]
     , Components.button (save model) [] "Save"
    ]




view : Model -> Html msg
view model =
  View.viewVoice model



main = text ""
