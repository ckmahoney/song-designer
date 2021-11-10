module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types as T
import Data as D
import Update as U


view : T.State T.SynthPreset -> Html U.UpdateMsg
view model =
    div []
        [ h1 [] [ text "SPA in Elm" ]
        , div []
            [ text "Button pushed in this screen: "

            ]
        , div []
            [ button [  ] [ text "++" ]
            ]
        ]

cardTitle : String -> Html a
cardTitle title =
  div [class "is-size-3"] [text title]


cardImage : String -> Html msg
cardImage fp =
  div [class "card-image"]
    [ figure [class "image is-4by3"] 
       [ img [src fp, alt ""] [] ] ]

cardContent : T.SynthRole -> String -> String-> Html a
cardContent role title content = 
  div [class "card-content"]
    [ div [class "media"]
      [ div [class "media-left"]
         [ figure [class "image is-48x48"] [roleIcon role]]

         , div [class "media-content"]
             [ p [class "title is-4"] [text title]
             , p [class "subtitle is-6"] [text title] ] ]
      , div [class "content"] [text content] ]

card : T.SynthRole -> String ->  String ->  Html msg
card role label title  = 
  div [class "column card"]
    [ cardTitle label
    , cardContent role title ""]

roleIcon : T.SynthRole -> Html msg
roleIcon role =
  img [width 50, height 50, src <| "/svg/" ++ (Tuple.first <| D.roleLabel role) ++ ".svg"] []


roleRow : Html msg
roleRow = 
  div [] (List.map roleIcon D.roles)


roleCard : T.SynthRole -> Html msg
roleCard role = 
  let 
    text = D.roleLabel role
  in
  card role (Tuple.second text) ("Role for " ++ (Tuple.first text))


buttonOpt : T.SynthRole -> msg -> Html msg
buttonOpt role msg =
  button [onClick msg] [text (Tuple.first (D.roleLabel role))]


main =
  Html.text ""
