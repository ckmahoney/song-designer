module Main exposing (main)

import Browser
import Dict
import Html exposing (Html, button, div, text, label, p)
import Html.Events exposing (onClick)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = Dict.Dict String Int


type alias ScoreMeta =
  { cps : Float
  , root : Float
  , nCycles : Int
  }


init : Model
init =
  Dict.fromList
    [ ("density", 0)
    , ("complexity", 0)
    ]


toInt : Maybe Int ->  Int
toInt x =
    case x of 
      Nothing ->
         0
      Just i -> 
        i


-- UPDATE


type Msg
  = InsertInt String Int
  | ChangeNum String (Maybe Int)


update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeNum field x ->
      case x of 
        Nothing ->
          model
        Just n ->
          update (InsertInt field n) model

    InsertInt field n ->
      Dict.insert field n model


-- VIEW


intField : String -> String -> Int -> Html Msg
intField name field val = 
  div []
    [ label [] [ text name ]
    , p [] [text (String.fromInt val)]
    , button [ onClick (InsertInt field (val + 1)) ] [ "Less " ++ name |> text ]
    , button [ onClick (InsertInt field (val - 1)) ] [ "More " ++ name |> text ]
    ]


view : Model -> Html Msg
view model =
  div []
    [ intField "Density" "density" (toInt (Dict.get "density" model))
    , intField "Complexity" "complexity" (toInt (Dict.get "complexity" model))
    ]
