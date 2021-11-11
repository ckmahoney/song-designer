module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Types as T
import Data as D
import Update as U


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
         [ figure [class "image is-96x96"] [roleIcon role]]

         , div [class "media-content"]
             [ p [class "title is-4"] [text title]
             , p [class "subtitle is-6"] [text title] ] ]
      , div [class "content"] [text content] ]


card : T.SynthRole -> String ->  String ->  Html msg
card role label title  = 
  div [class "column card"]
    [ cardTitle label
    , cardContent role title ""]


backgroundInvert : String -> Html msg -> Html msg
backgroundInvert color el =
  div [style "background" color]
    [ div [style "filter" "invert(1)"] 
      [ el ] ]


roleIcon : T.SynthRole -> Html msg
roleIcon role =
  img [ width 50
      , height 50
      , src <| "/svg/" ++ (Tuple.first <| D.roleLabel role) ++ ".svg"] []


presetIcon : T.SynthRole -> Html msg
presetIcon role =
  div [class "box m-5", style "background" (D.roleColor role)]
  [ div [class "p-6"] [
      img [ width 50
          , height 50
          , src <| "/svg/" ++ (Tuple.first <| D.roleLabel role) ++ ".svg"] [] ] ]


roleThumb : T.SynthRole -> Html msg
roleThumb role =
  div [class "p-3 box", style "background" <| D.roleColor role]
    [ img [ style "filter" "invert(1)"
           , width 250
           , height 250
           , src <| "/svg/" ++ (Tuple.first <| D.roleLabel role) ++ ".svg"] [] ]


roleIconButton : (T.SynthRole -> U.UpdateMsg) -> T.SynthRole -> Html U.UpdateMsg
roleIconButton update role =
  div [class "column columns is-centered is-one-third "]
   [ div [class "box"]
     [ div [class "image is-48x48"]
       [ img  [ width 50
               , height 50
               , onClick (update role)
               , src <| "/svg/" ++ (Tuple.first <| D.roleLabel role) ++ ".svg"] [] ] ] ]

roleRow : Html msg
roleRow = 
  div [] (List.map roleIcon D.roles)


rolePicker : (T.SynthRole -> U.UpdateMsg) -> Html U.UpdateMsg
rolePicker update =
  div [class "mt-3 columns is-multiline is-centered margin-auto"] <| List.map (roleIconButton update) D.roles


roleCard : T.SynthRole -> Html msg
roleCard role = 
  let 
    text = D.roleLabel role
  in
  card role (Tuple.second text) (" " ++ (Tuple.first text))
  

editInt : String -> String -> Int -> (Int -> msg) -> Html msg
editInt title name val toMsg =
  div [class "level"]
    [ button [class "button", onClick (toMsg <| val - 1)] [text  "-" ]
    , label [] [text name, b [] [" " ++ String.fromInt val |> text]]
    , button [class "button", onClick (toMsg <| val + 1)] [text  "+ "]]


editTexture : T.SynthPreset -> Html U.UpdateMsg
editTexture preset =
  let 
    textD = D.textureLabel T.Density
    textC = D.textureLabel T.Complexity
  in 
  div [class "is-two-thirds"]
    [ editInt (Tuple.second textD) (Tuple.first textD) preset.density U.UpdateDensity 
    , editInt (Tuple.second textC) (Tuple.first textC) preset.complexity U.UpdateComplexity
    ]


buttonOpt : T.SynthRole -> msg -> Html msg
buttonOpt role msg =
  button [] [text (Tuple.first (D.roleLabel role))]


saveButton : T.SynthPreset -> Html U.UpdateMsg
saveButton synth =
  button [class "button", onClick (U.SavePreset synth)] [text "Clone"]

killButton : T.SynthPreset -> Html U.UpdateMsg
killButton synth =
  button [class "button", onClick (U.KillPreset synth)] [text "Delete"]


crudButtons preset =
  div [class "column is-flex is-justify-content-space-between" ]
    [ killButton preset
    , saveButton preset ]


synthDescription preset =
  div [class "column media-left"]
    [ div [class "media-content"]
    [ p [class "title is-4"] [text preset.title]
    , p [class "content is-6"] [text <| D.roleDescription preset.role] ] ]


texEdit preset =
  div [class "texture-editor column is-two-thirds mx-auto is-centered"] 
    [ editTexture preset ] 


synthCardContent : T.SynthPreset -> Html U.UpdateMsg
synthCardContent preset = 
  div [class "column card-content has-background-white "]
    [ div [class "column media is-centered p-3"]
      [ figure [class "image mx-auto is-96x96"] [roleThumb preset.role]]
      , synthDescription preset 
      , rolePicker (U.UpdateSynthRole preset) 
      , texEdit preset
      , crudButtons preset ]


synthCard : T.SynthPreset -> Html U.UpdateMsg
synthCard preset =
  div [class "column card is-one-third px-6 py-4" ,style  "background" (D.roleColor preset.role)]
    [ synthCardContent preset]


presetButton : T.SynthPreset -> Html U.UpdateMsg
presetButton preset = 
  div [ onClick (U.ChangeSelection preset) ] 
    [ presetIcon preset.role ]


carouselItem : Float -> Int -> Html msg -> Html msg
carouselItem dTheta i el =
  let 
    rotation = "rotate3d(0, 1, 0," ++ (String.fromFloat <| (toFloat i) * dTheta) ++ "deg)"
  in 
  div [class <| String.fromInt i, style "transform" rotation ] [el]


spiralItem : Int -> Float -> Int -> Html msg -> Html msg
spiralItem time dTheta i el =
  let 
    rotation = "rotate3d(0, 1, 0," ++ (String.fromFloat <| (toFloat i) * dTheta) ++ "deg)"
    amount =  100.0 + (100.0 * sin (toFloat (time // 100)))
    translation = String.replace "%" (String.fromFloat amount) "translate3d(0px,%px,0px)" 
    styleString = rotation ++ " " ++ translation
  in 
  div [ class <| String.fromInt i
      , style "transform" styleString ] [el]


modulateInt : Int -> Float
modulateInt time =
  sin <| ((toFloat time) / 1000)


carousel : Int -> List (Html msg) -> Html msg
carousel time children =
  let 
    dTheta = (modulateInt time) + 360.0 / (toFloat (List.length children))
    yy  = Debug.log "theta" dTheta
  in
  div [class "columns"]
    -- (List.indexedMap (\i el -> carouselItem dTheta i el) children)
    (List.indexedMap (\i el -> spiralItem time dTheta i el) children)


presetRow : List T.SynthPreset -> Html U.UpdateMsg
presetRow presets =
  div [class "level"] 
    <| List.map presetButton presets


testCarousel : Int -> Html msg
testCarousel time =
  let 
    items = (List.map roleThumb D.roles)
    moreItems = items ++ items ++ items
  in 
  carousel time moreItems  


synthEditor : T.State T.SynthPreset -> Html U.UpdateMsg
synthEditor model =
  div [class "columns"]
    [ testCarousel model.time ]
    -- , synthCard model.current
    -- , presetRow model.presets 


main  =
  Html.text ""
