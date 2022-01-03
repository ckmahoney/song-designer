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
import Data

type alias State = Scope


type Msg 
  = Save State
  | Update Field
  | Edit State
  | Close

type Field
  = Title String
  | CPS Float
  | CPC Int
  | Root Int
  | Size Int


type Model
  = Overview State
  | Editing State


change : State -> Field -> State
change state field =
  case field of 
    Title title ->
      { state | label = title }

    CPS cps ->
      { state | cps = cps }

    CPC cpc ->
      { state | cpc = cpc }

    Root root ->
      { state | root = root }

    Size size ->
      { state | size = size }


update : Msg -> State -> Model
update msg state = 
  case msg of 
    Close -> 
      Overview state

    Save next -> 
      Overview next
    
    Edit curr ->
      Editing curr

    Update field ->
      Editing <| change state field


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
  , tempos = [5/10, 1, 14/10, 22/10, 33/10, 44/10]
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

sizeOptions : List Int
sizeOptions =
  List.range 1 8


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


updateTitle :  String -> (Msg -> msg) -> msg
updateTitle  str msg =
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

sizePicker : Int -> (Msg -> msg) -> Html msg
sizePicker current msg =
  Components.boxAttrs border
   <| [ label [Attr.class "mr-3"] [ text "Size"]
    , div [Attr.class flexStyles] <| List.map (\size ->
    Components.button (msg (Update <| Size size)) [Attr.class <| if current == size then "is-success is-selected" else ""] (String.fromInt size)) sizeOptions ]


icon : Model -> Html msg
icon model =
  text "icon"


card :State -> (Msg -> msg) -> Html msg
card model toMsg = 
  Components.box
   [  Components.colsMulti 
        [ Components.colSize "is-half" <| div [] 
          [ p [ Attr.class "subtitle"] [ text model.label ]
          , V.sizeMessage model.cpc  model.size
          ]
        , Components.colSize "is-half" <| div [] 
          [ Components.editText "" (text "") model.label (\str -> updateTitle str toMsg)
          ]
        , Components.colFull <| div [] 
          [ p [Attr.class "has-text-centered"] [text ("duration: " ++ (V.timeString <| V.duration model.cpc model.cps model.size))]
          , p [Attr.class "has-text-centered"] [text <| "Key of " ++ V.keyLabel model.root]
          ]
        ]
    ]


card2 :State -> (Msg ->msg ) -> Html msg
card2 model toMsg = 
  Components.box <| 
   [  Components.cols <|
        [ Components.colSize "is-three-quarters" <| div [] 
          [ p [ Attr.class "subtitle"] [ text model.label ]
          , V.sizeMessage model.cpc  model.size
          , Components.editText "" (text "") model.label (\str -> updateTitle str toMsg)
          ]
        , Components.colSize "is-one-quarter" <| div [] 
          [ p [Attr.class "has-text-centered"] [text <| V.durString model.cpc model.cps model.size ++ " seconds long"]
          , p [Attr.class "has-text-centered"] [text <| "Key of " ++ V.keyLabel model.root]
          ]
        ]
    ]


thumb : State -> Html msg
thumb state =
  Components.box 
    [ div [Components.centerText] [ Components.label state.label ]
    , Components.colsWith [Components.centerText]  <|
       [ Components.colHalf <| text <| V.scopeTimeString state
       , Components.colHalf <| text <| Components.keyMessage True state.root
       ]
    ] 


brief : State -> msg -> Html msg
brief state open =
  Components.box 
    [ Components.label "Scope"
    , Components.cols <|
       [ Components.colSize "is-three-quarters" <| div [] [ Components.label state.label       , Components.colHalf <| text <| V.scopeTimeString state ]
       , Components.colSize "is-one-quarter" <| Components.svgButtonClass "settings" "has-background-primary" open
       ]
    , p [ Attr.class "content" ] [ V.sizeMessage state.cpc state.size ]   
    ] 


info : State -> Html msg
info state =
  Components.box 
    [ Components.cols <| List.map (\(label, content) -> Components.col [] [text label, text content])
      [ ("Key", Components.keyMessage True state.root)
      , ("Phrasing", String.fromInt state.cpc)
      , ("Tempo", String.fromFloat state.cps)
      , ("Size", String.fromInt state.size)
      , ("Duration", V.durString  state.cpc state.cps state.size)
      ]


    ] 


editorMobile : (Msg -> msg) -> State ->  Html msg
editorMobile toMsg state  =
  Components.cols
    [ card2 state toMsg
    , keyPicker state.root toMsg
    , cpsPicker state.cps toMsg
    , cpcPicker state.cpc toMsg
    , sizePicker state.size toMsg
    -- , Components.button (toMsg <| Save state) [] "Save Scope" 
    ]


editorDesktop : (Msg -> msg) -> State -> Html msg
editorDesktop toMsg state  =
   div [Attr.class "is-flex is-flex-direction-column view1"]
     [  card state toMsg
        , Components.card "Scope label" <| div []
        [ p [] [text "Something like 'verse' or 'chorus' to help you what this part is doing."]
        , input [Attr.class "input my-3 is-info", Attr.type_ "text",  Attr.value state.label, onInput (\str -> toMsg (Update <| Title  str))] [] ]
     , keyPicker state.root toMsg
     , cpsPicker state.cps toMsg
     , cpcPicker state.cpc toMsg
     , sizePicker state.size toMsg
     ]


editor : State -> (Msg -> msg) -> Html msg
editor state toMsg =
  div []
   [ Components.mobileOnly <| editorMobile toMsg state
   , Components.tabletOnly <| editorDesktop toMsg state
   , Components.desktopOnly <| editorDesktop toMsg state 
   ]

initFrom : State -> Model
initFrom state =
  Overview state


view : Model -> (Model -> msg) -> (State -> msg) -> (State -> msg) -> Html msg
view model forward save close =
  case model of 
    Overview state ->
     div []
      [ text "peeking a scope readonly"
      , thumb state
      ]

    Editing state ->
     let
       toMsg = (\msg -> forward <| update msg state)
     in
      div []
       [ Components.sectionHeading "scope" (Data.helpLink "scope") "Scope Editor" [ Components.saveButton (close state) "Save Scope" ]
      
       , editor state toMsg
       ]

main = 
  Html.text ""
