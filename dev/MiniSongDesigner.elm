module MiniSongDesigner exposing (..)

import Browser
import Html exposing (Html, button, div, text, label, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Types as T
import Data
import View 
import Components
import Tools
import Array
import Http
import Url.Builder as Url
import Json.Decode as Decode
import Json.Encode as Encode

import ScopeEditor
import EnsembleEditor
import ComboEditor
import LayoutEditor


type alias State = List T.Combo 

-- data prepared for storage on server
type Msg 
  = Open State
  | Close State
  | Select State Int T.Combo
  | Update State Int LayoutEditor.EditState
  | UpdateTitle (String)



type alias Model =
  { index : Int
  , member : Maybe T.GhostMember
  , layout : State
  , layoutEditor : Maybe LayoutEditor.Model
  , title : String
  }




initModel : Model
initModel =
  Model -1  Nothing LayoutEditor.initState LayoutEditor.initModel ""



initFromMember : T.GhostMember -> Model
initFromMember member = 
  let
    rec = { initModel | member = Just member }
  in 
  rec


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateTitle title ->
      ({ model | title = title }, Cmd.none)

    Close layout ->
      ({ model | layout = layout, layoutEditor = Nothing }, Cmd.none)

    Open layout ->
      ({ model |  layoutEditor = Just <| LayoutEditor.Overview layout}, Cmd.none)

    Select layout index combo ->
      ({ model | layoutEditor = Just <| LayoutEditor.Editing layout index (LayoutEditor.Open combo) }, Cmd.none)

    Update layout index editState ->
      case editState of 
        LayoutEditor.Open combo ->
         let
           next = Tools.replaceAt index combo layout
         in
          ({ model | layoutEditor = Just <| LayoutEditor.Editing next index (LayoutEditor.Open combo)}, Cmd.none)

        LayoutEditor.Scope editor ->         
            ({ model | layoutEditor = Just <| LayoutEditor.Editing layout index <| LayoutEditor.Scope editor}, Cmd.none)

        LayoutEditor.Ensemble editor ->
          case editor of 
            EnsembleEditor.Overview ensemble ->
              ({ model | layoutEditor = Just <| LayoutEditor.Editing layout index <| LayoutEditor.Ensemble  editor}, Cmd.none)

            EnsembleEditor.Editing ensemble voiceIndex voice ->
              ({ model | layoutEditor = Just <| LayoutEditor.Editing layout index <| LayoutEditor.Ensemble  editor}, Cmd.none)



view : (Msg -> msg) -> (Maybe Model) -> Html msg
view toMsg mmodel =
  case mmodel of 
      Nothing -> 
       Components.button (toMsg <| Open []) [] "Edit your layout"

      Just model ->
        case model of 
          LayoutEditor.Overview layout -> 
           let 
            select = (\i -> toMsg <| Select layout i <| Tools.getOr i layout Data.emptyCombo)
            kill = (\i -> toMsg <| Open (Tools.removeAt i layout))
           in 
           div [] 
            [ Components.button (toMsg <| Close layout) [] "Close"
            , Components.editText "Title" (text "The name for this sound") model.title (\str -> toMsg <| UpdateTitle str)
            , LayoutEditor.view layout select kill
            , if 4 > (List.length layout) then 
                 Components.button (toMsg <| (Open <| List.reverse <| Data.emptyCombo :: layout)) [] "Add another Combo" else text ""
            ]

          LayoutEditor.Editing layout index stateModel -> 
           let
             up = (\eState -> toMsg <| Update layout index eState)
             combo = Tools.getOr index layout Data.emptyCombo
             done = (toMsg <| Open layout)
           in
            div []
              [ Components.button (toMsg <| Close layout) [] "Close"
              , LayoutEditor.edit stateModel index combo up done
              ]


main = text ""
