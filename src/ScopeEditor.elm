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

type alias State = Scope

type Msg 
  = Close State
  | UpdateTitle State String
  | UpdateCPS State Float
  | UpdateRoot State Int
  | Edit State


type Model
  = Overview State
  | Editing State


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
  , tempos = [ 9/10, 4/3, 2.25 ]
  }


-- Options for the Mini Song Designer
rootOptions = 
  [ (0, "C")
  , (4, "E")
  , (8, "G#")
  ]


initScope = Scope 0 "Teaser" 1.25 4 4 1


initState : State
initState =
  { id = -1
  , label = "my delight"
  , cps = 4/3
  , cpc = 4
  , root = 4
  , size = 1
  }


initModel = Overview initState


init : (Model, Cmd Msg)
init = 
  (initModel, Cmd.none)


cpsOptions : List Float
cpsOptions =
  bounds.tempos


updateTitle : State -> String -> (Msg -> msg) -> msg
updateTitle state str msg =
  msg (UpdateTitle state str)


keyPicker : Int -> State -> (Msg -> msg) -> Html msg
keyPicker current state msg =
  Components.boxWith "is-flex is-align-items-center"
   <| (label [Attr.class "mr-3"] [ text "Key"]) :: List.map (\(root, name) ->
    Components.button (msg (UpdateRoot state root)) [Attr.class <| if current == root then "is-success is-selected" else ""] name) rootOptions
  

cpsPicker : Float -> State -> (Msg -> msg) -> Html msg
cpsPicker current state msg =
  Components.boxWith "is-flex is-align-items-center"
   <| (label [Attr.class "mr-3"] [ text "BPM"]) :: List.map (\cps ->
    Components.button (msg (UpdateCPS state cps)) [Attr.class <| if current == cps then "is-success is-selected" else ""] (String.fromFloat (Tools.cpsToBPM cps))) cpsOptions


view : (Msg -> msg) -> Model -> Html msg
view toMsg model =
  case model of 
    Overview state ->
      div [onClick (toMsg <| Edit state)] [Elements.scope state] 

    Editing state ->
      Components.box
        [ input [Attr.class "input my-3 is-info", Attr.type_ "text",  Attr.value state.label, onInput (\str -> (updateTitle state str toMsg))] []
        , cpsPicker state.cps state toMsg
        , keyPicker state.root state toMsg
        , Components.button (toMsg <| Close state) [] "Done" 
        ]


main = 
  Html.text ""
