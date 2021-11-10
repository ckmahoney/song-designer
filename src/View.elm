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


roleIconButton : (T.SynthRole -> U.UpdateMsg) -> T.SynthRole -> Html U.UpdateMsg
roleIconButton update role =
  img [ width 50
      , height 50
      , onClick (update role)
      , src <| "/svg/" ++ (Tuple.first <| D.roleLabel role) ++ ".svg"] []


roleRow : Html msg
roleRow = 
  div [] (List.map roleIcon D.roles)


rolePicker : (T.SynthRole -> U.UpdateMsg) -> Html U.UpdateMsg
rolePicker update =
  div [] 
    <| List.map (roleIconButton update) D.roles


roleCard : T.SynthRole -> Html msg
roleCard role = 
  let 
    text = D.roleLabel role
  in
  card role (Tuple.second text) (" " ++ (Tuple.first text))
  

editInt : String -> String -> Int -> (Int -> msg) -> Html msg
editInt title name val toMsg =
  div []
    [ label [] [text name]
    , b [] [text <| String.fromInt val]
    , button [onClick (toMsg <| val - 1)] [text <| "Less " ++ name]
    , button [onClick (toMsg <| val + 1)] [text <| "More " ++ name]]


editTexture : T.SynthPreset -> Html U.UpdateMsg
editTexture preset =
  let 
    textD = D.textureLabel T.Density
    textC = D.textureLabel T.Complexity
  in 
  div []
    [ editInt (Tuple.second textD) (Tuple.first textD) preset.density U.UpdateDensity 
    , editInt (Tuple.second textC) (Tuple.first textC) preset.complexity U.UpdateComplexity
    ]


buttonOpt : T.SynthRole -> msg -> Html msg
buttonOpt role msg =
  button [] [text (Tuple.first (D.roleLabel role))]


synthCardContent : T.SynthPreset -> Html U.UpdateMsg
synthCardContent synth = 
  div [class "card-content"]
    [ div [class "media"]
      [ div [class "media-left"]
         [ figure [class "image is-48x48"] [roleIcon synth.role]]

         , div [class "media-content"]
             [ p [class "title is-4"] [text synth.title]
             , p [class "subtitle is-6"] [text synth.title] ] ]

      , div [class "content"] 
          [ text "Update your Synth"
          , editTexture synth ] ]


synthCard : T.SynthPreset -> Html U.UpdateMsg
synthCard preset =
  div [class "column card"]
    [ cardTitle "Synth Design"
    , synthCardContent preset
    , rolePicker (U.UpdateSynthRole preset) ]


main  =
  Html.text ""
