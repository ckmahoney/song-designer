module EnsembleEditor exposing (..)


import Browser
import Html exposing (Html, button, div, text, label, p, input)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)


import Defs.Types exposing (..)
import Defs.Data
import Components.View as View
import Elements
import Tools
import Components.Components as Components
import Editor.Voice as VoiceEditor


type alias Child = Voice
type alias State = List Child


type Msg
  = Create SynthRole
  | Select Int 
  | Update Int Voice
  | Kill Int
  | Save State
  | Picking SynthRole

type alias PickingSynth = (Maybe SynthRole)


type Model 
  = Overview State PickingSynth
  | Editing State Int Child


update : Msg -> State -> Model
update msg state =
  case msg of 
    Picking role ->
      Overview state (Just role)

    Save next ->
      Overview next Nothing

    Create role ->  
     let next = VoiceEditor.newVoice in 
      Overview ( { next | role = role } :: state) Nothing

    Select voiceIndex -> 
      Editing state voiceIndex <| Tools.getOr voiceIndex state VoiceEditor.newVoice

    Update index voice -> 
     let 
       next = Tools.replaceAt index voice state
      in
      Editing next index voice

    Kill index  -> 
      Overview  (Tools.removeAt index state) Nothing


brief : State -> msg -> Html msg
brief state open =
 let 
  child = Components.cols <| if List.length state == 0 then 
    [ text "No voices in this ensemble. Click the settings icon to add a voice." ] else 
         [ Components.col [Attr.class "is-multiline columns is-mobile"] <| 
           List.map (\{role} -> Components.colSize "is-one-third is-flex is-justify-content-center" <| View.roleIconColored role) state

         ]
 in 
  Components.box <|
    [ Components.colsWith [Attr.class "is-mobile"] <|
      [ Components.col1 <| Components.label "Ensemble"
      , Components.colSize "is-one-quarter" <| Components.svgButtonClass "settings" "has-background-primary" open 
      ]
    , Components.mobile [] <| List.singleton <| child
    , Components.tablet [] <| List.singleton <| child
    , Components.desktop [] <| List.singleton <| child
    ]


colors : State -> List String
colors ensemble =
  List.map (.role >> Data.roleColor) ensemble


thumb : State -> Html msg
thumb state  =
 let 
  child = Components.cols 
         [ Components.col [Attr.class "is-multiline columns is-mobile"] <| 
           List.map (\{role} -> Components.colSize "is-one-third is-flex is-justify-content-center" <| View.roleIconColored role) state
         ]
 in 
  Components.box <| if List.length state == 0 then 
    [ div [] [ text "They say silence is golden. You'll get a lot of gold with no voices here." ]
    , child
    ]
    else 
     [ Components.mobile [] <| List.singleton <| child
     , Components.tablet [] <| List.singleton <| child
     , Components.desktop [] <| List.singleton <| child
    ]


initModel : Model
initModel = 
  Overview [] Nothing


-- editor : (Msg -> msg) -> State -> Int -> VoiceEditor.Model -> Html msg
-- editor toMsg state index model =
--   let
--     up = (\v -> toMsg <| Update index v)
--     close = (\v -> toMsg <| Save state)
--     kill = toMsg <| Kill index
--   in 
--   VoiceEditor.edit model up close kill


voiceGrid : State -> Voice -> (Msg -> msg) -> Html msg
voiceGrid state curr toMsg =
  let
    next = List.append state [ Data.emptyVoice ]
    i = List.length next
  in 
  Components.box <|
   List.append [ (Components.label "Ensemble") ]
   <| List.singleton <| Components.colsWith [Attr.class "is-multiline is-vcentered has-text-centered"] <|
    List.indexedMap (\index ({role} as el) -> 
         div [Attr.class <| if el == curr then "has-background-warning" else "",  Attr.class "column is-one-quarter", onClick <| toMsg <| Select index] [View.roleIconColored role]) state 


editorNew :State -> Int -> Voice -> (Msg -> msg) -> Html msg
editorNew state index voice toMsg =
  let
    up = (\v -> toMsg <| Update index v)
    close = (\v -> toMsg <| Save state)
    kill = toMsg <| Kill index
  in 
  div []
    [ VoiceEditor.edit voice up close kill
    , voiceGrid state voice toMsg
    ]


showNoVoices : State -> (Msg -> msg) -> Html msg
showNoVoices state toMsg =
  Components.button (toMsg <| Create Bass) [Attr.class "is-success"] "Create the First Voice for this Ensemble"  


controls : (Msg -> msg) -> Int -> Voice -> Html msg
controls toMsg i voice =
  div [Attr.class "column is-flex is-flex-direction-column is-one-quarter"] 
    [ div [ Attr.class "column is-flex is-align-items-flex-start is-justify-content-center" ] <|
        [ Components.svgButtonClass "settings" "has-background-primary" (toMsg <| Select  i) ]
    , div [ Attr.class "column is-flex is-align-items-flex-end is-justify-content-center" ] <|
        [Components.svgButtonClass "trash" "has-background-danger" (toMsg <| Kill  i)]
    ]


voiceCard : (Msg -> msg) -> Int -> Voice -> Html msg
voiceCard toMsg i voice =
  div [ Attr.class "columns is-flex my-3", Attr.style "border" "1px solid lightgrey",  Attr.style "border-radius" "5px" ]  
     [ Components.col1 <| VoiceEditor.view voice
     ,  controls toMsg i voice
     ]
 

listVoices : State -> (Msg -> msg) -> Html msg
listVoices state toMsg =
  div [Attr.class "is-full has-text-centered"] <|
   List.indexedMap (voiceCard toMsg) state 


synthInitializer : PickingSynth -> (Msg ->msg) -> (SynthRole -> msg) -> msg ->Html msg
synthInitializer model toMsg select init =
  case model of 
  Nothing ->
    Components.addButton init "Add a Voice"
  Just curr ->
    let
      pick = (\i -> select <| Tools.getOr i Data.roles curr)
    in
    Components.box <| 
      [ Components.pickerSelected Data.roles View.roleIconColored pick curr
      , Components.button (toMsg <| Create curr) [Attr.class "has-background-success has-text-light is-size-5"] ("Create " ++ Data.roleName curr)
      ]


view : Model -> (Model -> msg) -> (State -> msg) -> (State -> msg) -> Html msg
view model forward save close =
 case model of 
  Overview state mSynth ->
   let
     toMsg = (\msg -> (forward <| update msg state))
     select = (\role -> toMsg <| Picking role)
   in 
    if 0 == List.length state then 
      showNoVoices state toMsg
    else
     Components.colsMulti
        [ Components.colFull <| Components.cols <|
          [ Components.sectionHeading "ensemble" (Data.helpLink "ensemble") "Ensemble Designer" [ Components.saveButton (save state) "Save Ensemble"] ]
        , Components.colFull <| synthInitializer mSynth toMsg select (select Bass) 
        , Components.colSize "is-full is-flex is-flex-direction-column align-items-flex-start" <|
           listVoices state toMsg 
        ]

  Editing state index voice ->
    div [] [ 
      editorNew state index voice (\msg -> (forward <| update msg state))
    ]


main = text ""
