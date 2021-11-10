module Main exposing (main)

import View
import Browser
import Dict
import Types as T
import Data exposing (p1)
import Html exposing (Html, button, div, text, label, p)
import Html.Events exposing (onClick)


-- MAIN


-- MODEL


type alias Model = 
  T.SynthPreset




type Msg
  = Done
  | UpdateSynth T.SynthRole


init : Model
init =
   p1


-- UPDATE



update : Msg -> Model -> Model 
update msg model =
  case msg of
    Done ->
      model
    UpdateSynth r ->
      {model | role = r}


toInt : Maybe Int ->  Int
toInt x =
    case x of 
      Nothing ->
         0
      Just i -> 
        i


-- UPDATE


-- VIEW



view : Model -> Html Msg
view model =
  div [] 
    ([div [] [text (Tuple.second <| Data.roleLabel model.role)]] 
     ++ (List.map (\r -> View.buttonOpt r (UpdateSynth r)) [T.Kick, T.Perc, T.Hat]))

    -- [ intField "Density" "density" (toInt (Dict.get "density" model))
    -- , intField "Complexity" "complexity" (toInt (Dict.get "complexity" model))
    -- , 
    -- ] 


main =
  Browser.sandbox { init = init, update = update, view = view }


