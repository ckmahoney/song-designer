module ScopeEditor exposing (..)


import Browser
import Html exposing (Html, button, div, text, label, p, input)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)

import Types exposing (..)
import View as V
import Elements
import Tools
import Components


type alias State = Scope


type Msg 
  = Save State
  | Update Field
  | Edit State

type Field
  = Title String
  | CPS Float
  | CPC Int
  | Root Int
  | Size Int


type Model
  = Overview State
  | Editing State


update : Msg -> State -> Model
update msg state = 
  case msg of 
    Save next -> 
      Overview next
    
    Edit s ->
      Editing s

    Update field ->
      case field of 
        Title title ->
          Editing { state | label = title }

        CPS cps ->
          Editing { state | cps = cps }

        CPC cpc ->
          Editing { state | cpc = cpc }

        Root root ->
          Editing { state | root = root }

        Size size ->
          Editing { state | size = size }


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
  msg (Update <| Title str)

flexStyles = "is-flex is-justify-content-space-around is-flex-wrap-wrap"


keyPicker : Int -> (Msg -> msg) -> Html msg
keyPicker current msg =
  Components.boxAttrs border
   <| [ label [Attr.class "mr-3"] [ text "Key"]
      ,  div [Attr.class flexStyles] <| List.map (\(root, name) ->
    Components.button (msg (Update <| Root root)) [Attr.class <| if current == root then "is-success is-selected" else ""] name) rootOptions]
  

cpsPicker : Float -> (Msg -> msg) -> Html msg
cpsPicker current msg =
  Components.boxAttrs border
   <| [ label [Attr.class "mr-3"] [ text "BPM"]
      , div [Attr.class flexStyles] <| List.map (\cps ->
    Components.button (msg (Update <| CPS cps)) [Attr.class <| if current == cps then "is-success is-selected" else ""] (String.fromFloat (Tools.cpsToBPM cps))) cpsOptions ]

cpcPicker : Int -> (Msg -> msg) -> Html msg
cpcPicker current msg =
  Components.boxAttrs border
   <| [ label [Attr.class "mr-3"] [ text "Phrase Length"]
    , div [Attr.class flexStyles] <| List.map (\cpc ->
    Components.button (msg (Update <| CPC cpc)) [Attr.class <| if current == cpc then "is-success is-selected" else ""] (String.fromInt cpc)) cpcOptions ]


icon : Model -> Html msg
icon model =
  text "icon"


card :State -> Html msg
card model = 
  Components.cardWith "has-background-warning" model.label <| Components.cols 
   [  V.sizeMessage model.cpc  model.size
    , p [Attr.class "has-text-centered"] [text <| "Key of " ++ V.keyLabel model.root]
    , Html.br [] [] 
    , Html.hr [] [] 
    , Html.br [] [] 
    , p [Attr.class "has-text-centered"] [text <| V.durString model.cpc model.cps model.size ++ " seconds long"]
    ] 


thumb : State -> Html msg
thumb state =
  Components.box 
    [ Components.cols <|
       [ Components.colHalf <| Components.label state.label
       , Components.colHalf <| text <| V.scopeTimeString state
       ]
    , p [ Attr.class "content" ] [ V.sizeMessage state.cpc state.size ]   
    ] 



editorMobile : (Msg -> msg) -> State ->  Html msg
editorMobile toMsg state  =
  Components.cols
    [ card state
    , Components.card "Scope label" <| div border
      [ p [] [text "Something like 'verse' or 'chorus' to help you what this part is doing."]
      , input [Attr.class "view0 input my-3 is-info", Attr.type_ "text",  Attr.value state.label, onInput (\str -> toMsg (Update <| Title  str))] [] ]
    , cpsPicker state.cps toMsg
    , cpcPicker state.cpc toMsg
    , keyPicker state.root toMsg
    , Components.button (toMsg <| Save state) [] "Save Scope" 
    ]


editorDesktop : (Msg -> msg) -> State -> Html msg
editorDesktop toMsg state  =
   div [Attr.class "is-flex is-flex-direction-column view1"]
     [  card state
        , Components.card "Scope label" <| div []
        [ p [] [text "Something like 'verse' or 'chorus' to help you what this part is doing."]
        , input [Attr.class "input my-3 is-info", Attr.type_ "text",  Attr.value state.label, onInput (\str -> toMsg (Update <| Title  str))] [] ]
     , cpsPicker state.cps toMsg
     , cpcPicker state.cpc toMsg
     , keyPicker state.root toMsg
     ]


editor : State -> (Msg -> msg) -> Html msg
editor state toMsg =
  div []
   [ Components.button (toMsg <| Save state) [] "Save Scope"
   , Components.mobileOnly <| editorMobile toMsg state
   , Components.tabletOnly <| editorMobile toMsg state
   , Components.desktopOnly <| editorDesktop toMsg state 
   ]


view : Model -> (Msg -> msg) -> msg -> Html msg
view model toMsg close =
  case model of 
     Overview state ->
      div [onClick <| toMsg <| Edit state] [ thumb state ]

     Editing state ->
        editor state toMsg


main = 
  Html.text ""
