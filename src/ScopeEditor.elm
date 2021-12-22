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
  | UpdateCPC State Int
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
  , tempos = [5/10, 1, 14/10, 22/10, 33/10, 44/10  ]
  }


-- Options for the Mini Song Designer
rootOptions = 
  [ (0, "C")
  , (2, "D")
  , (5, "F")
  , (7, "G")
  , (10, "Bb")
  ]

cpcOptions : List Int
cpcOptions =
  [ 3, 4, 6, 8
  ]


initScope = Scope 0 "Teaser" 1.25 4 5 (44//10)

border = 
 [Attr.style "border" "1px solid lightgrey",  Attr.style "border-radius" "5px"]

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

flexStyles = "is-flex is-justify-content-space-around is-flex-wrap-wrap"


keyPicker : Int -> State -> (Msg -> msg) -> Html msg
keyPicker current state msg =
  Components.boxAttrs border
   <| [ label [Attr.class "mr-3"] [ text "Key"]
      ,  div [Attr.class flexStyles] <| List.map (\(root, name) ->
    Components.button (msg (UpdateRoot state root)) [Attr.class <| if current == root then "is-success is-selected" else ""] name) rootOptions]
  

cpsPicker : Float -> State -> (Msg -> msg) -> Html msg
cpsPicker current state msg =
  Components.boxAttrs border
   <| [ label [Attr.class "mr-3"] [ text "BPM"]
      , div [Attr.class flexStyles] <| List.map (\cps ->
    Components.button (msg (UpdateCPS state cps)) [Attr.class <| if current == cps then "is-success is-selected" else ""] (String.fromFloat (Tools.cpsToBPM cps))) cpsOptions ]

cpcPicker : Int -> State -> (Msg -> msg) -> Html msg
cpcPicker current state msg =
  Components.boxAttrs border
   <| [ label [Attr.class "mr-3"] [ text "Phrase Length"]
    , div [Attr.class flexStyles] <| List.map (\cpc ->
    Components.button (msg (UpdateCPC state cpc)) [Attr.class <| if current == cpc then "is-success is-selected" else ""] (String.fromInt cpc)) cpcOptions ]


icon : Model -> Html msg
icon model =
  text "icon"


card :State -> Html msg
card model = 
  Components.cardWith "has-background-warning" model.label <| Components.cols 
   [  View.sizeMessage model.cpc  model.size
    , p [Attr.class "has-text-centered"] [text <| "Key of " ++ View.keyLabel model.root]
    , Html.br [] [] 
    , Html.hr [] [] 
    , Html.br [] [] 
    , p [Attr.class "has-text-centered"] [text <| View.durString model.cpc model.cps model.size ++ " seconds long"]
    ] 


-- better for mobile
viewMobile : (Msg -> msg) -> Model -> msg -> Html msg
viewMobile toMsg model done =
  case model of 
    Editing state ->
      Components.cols
        [ card state
        , Components.card "Scope label" <| div border
          [ p [] [text "Something like 'verse' or 'chorus' to help you what this part is doing."]
          , input [Attr.class "view0 input my-3 is-info", Attr.type_ "text",  Attr.value state.label, onInput (\str -> toMsg (UpdateTitle state str))] [] ]
        , cpsPicker state.cps state toMsg
        , cpcPicker state.cpc state toMsg
        , keyPicker state.root state toMsg
        , Components.button (toMsg <| Close state) [] "Save Scope" 
        ]

    Overview state ->
      text "Thanks for finding this bug, can you please tell me about it? How did you get here?"


-- better for desktop
view1 : (Msg -> msg ) -> State  -> Html msg
view1 toMsg state  =
   div [Attr.class "is-flex is-flex-direction-column view1"]
     [  card state
        , Components.card "Scope label" <| div []
        [ p [] [text "Something like 'verse' or 'chorus' to help you what this part is doing."]
        , input [Attr.class "input my-3 is-info", Attr.type_ "text",  Attr.value state.label, onInput (\str -> toMsg (UpdateTitle state str))] [] ]
     , cpsPicker state.cps state toMsg
     , cpcPicker state.cpc state toMsg
     , keyPicker state.root state toMsg
     ]



main = 
  Html.text ""
