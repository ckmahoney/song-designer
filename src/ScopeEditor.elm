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
  , tempos = [5/10, 14/10,  9/10, 22/10, 33/10, 44/10  ]
  }


-- Options for the Mini Song Designer
rootOptions = 
  [ (0, "C")
  , (2, "D")
  , (5, "F")
  , (7, "G")
  , (10, "Bb")
  ]


initScope = Scope 0 "Teaser" 1.25 4 5 (44//10)


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


icon : Model -> Html msg
icon model =
  text "icon"


card :State -> Html msg
card model = 
  Components.card model.label <| Components.cols 
   [  View.meterMessage model.cpc
    , View.sizeMessage model.cpc  model.size
    , text <| "Key of " ++ View.keyLabel model.root
    ] 



view : (Msg -> msg) -> Model -> msg -> Html msg
view toMsg model done =
  case model of 
    Editing state ->
      Components.cols
        [ Components.col1 <| card state
        , Components.col1 <| div []
          [ label [Attr.class "label" ] [text "Section label"]
          , p [] [text "Something like 'verse' or 'chorus' to help you remember where in the song this part is."]
          , input [Attr.class "input my-3 is-info", Attr.type_ "text",  Attr.value state.label, onInput (\str -> (updateTitle state str toMsg))] [] ]
        , cpsPicker state.cps state toMsg
        , keyPicker state.root state toMsg
        , Components.button (toMsg <| Close state) [] "Done" 
        ]

    Overview state ->
      Components.box
        [ card state
        , div []
          [ label [Attr.class "label" ] [text "Section label"]
          , p [] [text "Something like 'verse' or 'chorus' to help you remember where in the song this part is."]
          , input [Attr.class "input my-3 is-info", Attr.type_ "text",  Attr.value state.label, onInput (\str -> (updateTitle state str toMsg))] [] ]
        , cpsPicker state.cps state toMsg
        , keyPicker state.root state toMsg
        , Components.button (toMsg <| Close state) [] "Done2" 
        ]


main = 
  Html.text ""
