module ScopeEditor exposing (..)


import Browser
import Html exposing (Html, button, div, text, label, p, input)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)

import Types exposing (..)
import View 
import Elements
import Tools
import Components

type alias Bounds = 
  { minCPS : Float
  , maxCPS : Float
  , minCPC : Int
  , maxCPC : Int
  , minSeconds : Float
  , maxSeconds : Float
  , roots : List Float
  , tempos : List Float
  }


bounds : Bounds 
bounds = 
  { minCPS = 1.0
  , maxCPS = 4.0
  , minCPC = 4
  , maxCPC = 4
  , minSeconds = 1.0
  , maxSeconds = 15.0
  , roots = [0, 4, 8]
  , tempos = [ 0.5, 1.25, 2.5 ]
  }


rootOptions = 
  [ (0, "C")
  , (4, "E")
  , (8, "G#")
  ]


type alias State = Scope


initScope = Scope 0 "Teaser" 1.25 4 4 1


initState : State
initState =
  { id = -1
  , label = "my delight"
  , cps = 1.25
  , cpc = 4
  , root = 4
  , size = 1
  }


initModel = Overview initState


init : (Model, Cmd Msg)
init = 
  (initModel, Cmd.none)


type Msg 
  = Over State
  | EditTitle State String
  | UpdateCPS State Float
  | UpdateRoot State Int
  | Edit State

type Model
  = Overview State
  | EditingTitle State 
  | Editing State


cpsOptions : List Float
cpsOptions =
  bounds.tempos


updateTitle : State -> String -> (Msg -> msg) -> msg
updateTitle state str msg =
  msg (EditTitle state str)


keyPicker : State -> (Msg -> msg) -> Html msg
keyPicker state msg =
  Components.box
   <| (label [] [ text "Key:"]) :: List.map (\(root, name) ->
    Components.button (msg (UpdateRoot state root)) [] name) rootOptions
  

cpsPicker : State -> (Msg -> msg) -> Html msg
cpsPicker state msg =
  Components.box
   <| (label [] [ text "BPM:"]) :: List.map (\cps ->
    Components.button (msg (UpdateCPS state cps)) [] (String.fromFloat (Tools.cpsToBPM cps))) cpsOptions


view : (Msg -> msg) -> Model -> Html msg
view msg model =
  case model of 
    Overview state ->
      div [onClick (msg <| Edit state)] [Elements.scope state] 

    Editing state ->
      Components.box
        [ input [Attr.type_ "text",  Attr.value state.label, onInput (\str -> (updateTitle state str msg))] []
        , cpsPicker state msg
        , keyPicker state msg
        , Components.button (msg <| Over state) [] "Done" 
        ]

    EditingTitle state ->
      div [onInput (\str -> (updateTitle state str msg))]
       [ input [Attr.type_ "text",  Attr.value state.label] []
       , p [onClick <| msg <| Over state] [text "done"]
       ]


main = 
  Html.text ""
