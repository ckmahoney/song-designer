import Html exposing (Html, button, div, text, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Types as T exposing (Voice)
import Data
import Components
import Tools
import Array


type alias VoiceP =
  { duty : T.SynthDuty
  , role : T.SynthRole
  , label : String
  , density : Maybe Int
  , complexity : Maybe Int
  }


type alias Model = VoiceP


type VoiceEditor
  = Pick Int -- calls view, delete, create
  | View Voice 
  | Delete Voice  
  | Create -- calls edit
  | Edit VoiceP  -- calls update or patch
  | Update VoiceP  -- in memory storage
  | Patch VoiceP Voice -- external storage


type alias Update 
  = VoiceEditor


uComplexity : VoiceP -> Maybe Int -> VoiceP
uComplexity partial num = 
  case num of 
    Nothing ->
      partial
  
    Just n  -> 
      Update { partial | complexity  = n } 


uDensity : VoiceP -> Maybe Int -> VoiceP
uDensity partial num = 
  case num of 
    Nothing ->
      partial
  
    Just n  -> 
      Update { partial | density  = n } 


uLabel : VoiceP -> String -> VoiceP
uLabel partial str = 
  Update { partial | label = str } 


uRole : VoiceP -> T.SynthRole -> VoiceP
uRole partial r =
  Update { partial | role = r }
  

uDuty : VoiceP -> T.SynthDuty -> VoiceP
uDuty partial d =
  Update { partial | duty = d }
  

-- view for editing transient data. Requires signal for patching a record and signal to delete a record
editor : String -> (Html msg -> msg) -> (VoiceP -> msg) -> Html msg
editor label cols done =
  Components.card label <| 
   div [ class "container" ] 
    [ Components.colsWith [ class "is-mobile is-flex is-justify-content-space-between" ] cols ]


editVoice : List Voice -> Voice -> (Voice -> VoiceP -> msg) -> Voice -> Html msg
editVoice options voice patch delete =
  let 
    updateComplexity = uComplexity voice
    updateDensity = uDensity voice 
    updateLabel = uLabel voice
    updateRole = uRole voice
    updateDuty = uDuty voice
    -- options = List.map (\r -> (r, text .label r)) Data.roles
    html = 
       [ Components.editText "Label" (text "") voice.label updateLabel
       , Components.editSelection voice.role "Role" (text "") options voice.role updateRole 
       , Components.editToggle "Duty" (T.Structure, "Structural") (T.Expression, "Expressive") voice.duty updateDuty
       , Components.colsWith [ class "is-mobile is-flex is-justify-content-space-between" ]
         [ Components.colHalf <| Components.editInt "Density" (densityMessage voice) Data.rangeDensity voice.density updateDensity
         , Components.colHalf <| Components.editInt "Complexity" (complexityMessage voice) Data.rangeComplexity voice.complexity updateComplexity
         ]
       ]
  in
  editor "Voice" html (patch voice) delete


synthDescription : VoiceP -> Html msg
synthDescription preset =
  div [class "column media-left"]
    [ div [class "media-content"]
    [ p [class "title is-4"] [text preset.label]
    ,roleDescription preset.role ] ]


complexityMessage : VoiceP -> Html msg
complexityMessage {complexity} =
  if complexity < 2 then 
    text "Very basic harmonics."
  else if complexity < 5 then 
    text "Compelling harmonic motion without getting too far out."
  else
    text "Higher depths of harmony with ambiguous results."


densityMessage : VoiceP -> Html msg
densityMessage {density} =
  if density == 1 then 
    text "Very basic structure."
  else if density < 3 then 
    text "A good amount of structural variation for interest and stability."
  else
    text "A lot of motion, sometimes causing blurriness or obscurity."


roleDescription : T.SynthRole -> Html msg
roleDescription role =
  div [ class "content" ] [ Html.b [] [ text <| Tuple.first <| Data.roleLabel role],  p [] [text <| Data.roleDescription role ] ]
