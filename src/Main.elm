module Main exposing (main)

import View
import Browser
import Dict
import Types as T
import Data exposing (p1)
import Html exposing (Html, button, div, text, label, p)
import Html.Attributes as Attr
import Html.Events exposing (onClick)

type alias Model = 
  T.SynthPreset


type Msg
  = Done
  | UpdateSynth T.SynthRole


init : Model
init =
   p1

update : Msg -> Model -> Model 
update msg model =
  case msg of
    Done ->
      model
    UpdateSynth r ->
      {model | role = r}

toInt : Maybe Int ->  Int
toInt x =
  Maybe.withDefault 0 x

view : Model -> Html Msg
view model =
  div [] 
    ([ div [Attr.class "columns"] (List.map View.roleCard Data.roles) ]
     ++ (List.map (\r -> View.buttonOpt r (UpdateSynth r)) [T.Kick, T.Perc, T.Hat]))

    -- [ intField "Density" "density" (toInt (Dict.get "density" model))
    -- , intField "Complexity" "complexity" (toInt (Dict.get "complexity" model))
    -- , 
    -- ] 


main =
  Browser.sandbox { init = p1, update = update, view = view }


