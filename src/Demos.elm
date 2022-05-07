module Demos exposing (..)

import Browser
import Html exposing (Html, button, div, text, label, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Defs.Types as T
import Defs.Data
import Components.View as View 
import Components.Components as Components

type Model
  = None
  | Combo String
  | Layout String


type alias Init =
  { type_ : String
  , detail : String
  }

type Msg = 
  No

init : (Maybe Init) -> (Model, Cmd Msg)
init flags =
  case flags of 
    Nothing -> 
      (None, Cmd.none)
    
    Just {type_, detail} ->
      case type_ of 
        "combo" -> 
          (Combo detail, Cmd.none)

        "layout" ->
          (Layout detail, Cmd.none)

        _ ->
          (None, Cmd.none)


none : Html msg
none = 
  text ""

viewDemoLayout :  Model -> Html Msg
viewDemoLayout model = 
  View.viewLayout Data.demoLayout  

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

view : Model -> Html msg
view model =
  case model of
    None -> 
      text ""

    Combo selection ->
      case selection of 
        "intro" ->
          View.comboPreview Data.demoIntro

        "verse" ->
          View.comboPreview Data.demoVerse

        "chorus" ->
          View.comboPreview Data.demoChorus

        "outro" ->
          View.comboPreview Data.demoOutro

        _ ->
          text ""

    Layout str ->
      View.viewLayout Data.demoLayout
   
main = Browser.element 
  { init = init
   , update = update
   , view = view
   , subscriptions = (\_ -> Sub.none)
   }
