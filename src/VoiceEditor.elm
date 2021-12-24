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


numberPickers model update =
  Components.box <| List.singleton <|
  Components.cols <|  [ Components.colHalf <| Components.editInt "Density" (View.densityMessage model) Data.rangeDensity model.density (updateDensity model update)
     , Components.colHalf <| Components.editInt "Complexity" (View.complexityMessage model) Data.rangeComplexity model.complexity (updateComplexity model update)
    ]


controls model save kill = 
 div [] 
  [ Components.mobile [] <| List.singleton <|
     div [Attr.class "columns is-mobile"]
       [ div [Attr.class "column"] [ Components.svgButton "checkmark" (save model) ]
       , div [Attr.class "column has-text-right"] [Components.svgButton "trash" kill]
       ]
  , Components.tablet [] <| List.singleton <|
     div [Attr.class "columns px-1"] <|
       [ Components.col1 <| Components.svgButton "checkmark" (save model)
       , div [Attr.class "column is-half has-text-right"] [Components.svgButton "trash" kill]
       ]

  , Components.desktop [] <| List.singleton <|
     div [Attr.class "columns px-1"] <|
       [ Components.col1 <| Components.svgButton "checkmark" (save model)
       , div [Attr.class "column is-half has-text-right"] [Components.svgButton "trash" kill]
       ]

  ]


edit : Model -> (Model -> msg) -> (Model -> msg) -> msg -> Html msg
edit model update save kill =
 let
   options = List.map (\r -> (r, View.roleIcon r)) Data.roles
 in 
  div [ Attr.class "container" ]
    [ controls model save kill

    , Components.mobile [ Attr.class "is-mobile is-flex is-flex-wrap-wrap columns is-multiline" ] <|
          [ Components.colFull <| Components.editText "Label" (text "Name") model.label (updateLabel model update)
          , Components.colFull <| div [Attr.class "has-text-centered"] [ View.viewVoice model]
          , Components.colFull <| Components.cols <|
              [ Components.colHalf <| Components.editSelection model.role "" (text "Role") options model.role (updateRole model update)             
              , Components.colHalf <| numberPickers model update   
              ]
          ]

    , Components.tablet [ Attr.class "is-flex is-justify-content-space-between columns is-multilinbe" ] 

        [ Components.colHalf <| View.viewVoice model
        , Components.colHalf <| Components.colsMulti
            [ Components.editText "Label" (text "Name") model.label (updateLabel model update)
            , Components.editSelection model.role "" (text "Role") options model.role (updateRole model update)    
          , numberPickers model update   
            ]
        ]


    , Components.desktop [ Attr.class "columns is-multiline is-flex is-justify-content-space-between" ] 
        [ Components.colHalf <| View.viewVoice model
        , Components.colHalf <| Components.colsMulti
            [ Components.editText "Label" (text "Name") model.label (updateLabel model update)
            , Components.editSelection model.role "" (text "Role") options model.role (updateRole model update) 
          , numberPickers model update   
            ]

        ]
    ]


icon : Model -> Html msg
icon model = 
  View.voiceIcon model  


view : Model -> Html msg
view model =
  View.viewVoice model


main = text ""
