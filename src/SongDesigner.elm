module SongDesigner exposing (..)


import Browser
import Html exposing (Html, button, div, text, label, p, input)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)

import Types exposing (..)
import View 
import Elements
import Tools


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


type alias State =
  { title : String
  , cps : Float
  , cpc : Int
  , root : Float
  , scopes : List Scope
  } 


initState : State
initState =
  { title = "my delight"
  , cps = 1.25
  , cpc = 4
  , root = 32.0
  , scopes = [Scope 0 "Teaser" 1.25 4 4 1]
  }


initModel = Overview initState


init :  (Model, Cmd Msg)
init = 
  (initModel, Cmd.none)


type Msg 
  = Over State
  | EditTitle State String
  | UpdateCPS State Float


type Model
  = Overview State
  | EditingTitle State 



cpsOptions : List Float
cpsOptions =
  bounds.tempos


updateTitle : State -> String -> (Msg -> msg) -> msg
updateTitle state str msg =
  msg (EditTitle state str)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    Over state ->
      (Overview state, Cmd.none)

    EditTitle state title ->
      (EditingTitle state, Cmd.none)

    UpdateCPS state cps ->
      (Overview { state | cps = cps }, Cmd.none)


cpsPicker : State -> (Msg -> msg) -> Html msg
cpsPicker state msg =
  div [] <| List.map (\cps ->
    div [onClick <| msg (UpdateCPS state cps) ] [text <| String.fromFloat cps]) cpsOptions


view : (Msg -> msg) -> Model -> Html msg
view msg  model =
  case model of 
    Overview state ->
      div [] 
        [ div [ onClick (msg <| EditTitle state state.title) ] [text state.title, text "is it working?" ]
        , cpsPicker state msg
        , div [] <| List.map Elements.scope state.scopes ]

    EditingTitle state ->
      div [onInput (\str -> (updateTitle state str msg))]
       [ input [Attr.type_ "text",  Attr.value state.title] []
       , p [onClick <| msg <| Over state] [text "done"]
       ]



main = 
  Html.text ""
