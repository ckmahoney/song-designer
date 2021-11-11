module View exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)

import Types as T
import Data as D
import Update as U
import String exposing (String(..))

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
  div [class "box", style "background" (D.roleColor role)]
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
  div [class "column columns is-centered is-one-third mb-0"]
   [ div [class "box"]
     [ div [class "image is-48x48", style "box-shadow" ("0px 0px 30px " ++ D.roleColor role)]
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


-- Guarantees the integer will be in the range provided, inclusive
bindInt : Int -> Int -> Int ->  Int
bindInt min max val =
  if val > max then 
    max
  else if val < min then
    min
  else 
    val 


noClickButton : Html msg
noClickButton =
  button [ class "m-0 button image is-48x48 has-background-black"
         , style "border-radius" "50%"
         , style "cursor" "initial"] []
  

editInt : String -> String -> Int -> (Int -> msg) -> Html msg
editInt title name val toMsg =
  div [class "level"]
    [ button [class "button", onClick (toMsg <| val - 1)] [text  "-" ]
    , label [] [text name, b [] [" " ++ String.fromInt val |> text]]
    , button [class "button", onClick (toMsg <| val + 1)] [text  "+ "]]


editFloat : String -> String -> Float -> (Float -> msg) -> Html msg
editFloat title name val toMsg =
  div [class "level"]
    [ button [class "button", onClick (toMsg <| val - 1)] [text  "-" ]
    , label [] [text name, b [] [" " ++ String.fromFloat val |> text]]
    , button [class "button", onClick (toMsg <| val + 1)] [text  "+ "]]


editRange : String -> Html msg -> (Float, Float) -> Float -> Float -> (Float -> msg) -> Html msg
editRange title infoMsg (mn, mx) step_ val toMsg =
  div [class "m-3 level column box"
      , onInput (\s -> toMsg <| Maybe.withDefault 0.0 <| String.toFloat s) ]
    [ label [class "subtitle"] [text title]
    , input [type_ "range"
            , value <| String.fromFloat val
            , step <| String.fromFloat step_
            , Attr.min <| String.fromFloat mn
            , Attr.max <| String.fromFloat mx ] []
    , infoMsg ]


editBoundInt : String -> String -> (Int,  Int) -> Int -> (Int -> msg) -> Html msg
editBoundInt title name (min, max) val toMsg =
  let 
    less = if min == val then noClickButton else
             button [class "image button is-48x48", onClick (toMsg <| val - 1)] [text  "-" ]
    more = if max == val then noClickButton else 
             button [class "image button is-48x48", onClick (toMsg <| val + 1)] [text  "+ "]
  in 
  div [class "level"]
    [ less
    , label [] [text name, b [] [" " ++ String.fromInt val |> text]]
    , more ]

editBoundInt2 : String -> String -> (Int,  Int) -> Int -> (Int -> msg) -> Html msg
editBoundInt2 title name (min, max) val toMsg =
  let 
    less = if min == val then noClickButton else
             button [class "image button is-48x48", onClick (toMsg <| val - 1)] [text  "-" ]
    more = if max == val then noClickButton else 
             button [class "image button is-48x48", onClick (toMsg <| val + 1)] [text  "+ "]
  in 
  div [class "m-2 column box"]
    [ h5 [class "subtitle"] [text title]
    , div [] [ less
    , label [] [text name, b [] [" " ++ String.fromInt val |> text]]
    , more ] ]

editBoundFloat : String -> String -> (Float,  Float) -> Float -> (Float -> msg) -> Html msg
editBoundFloat title name (min, max) val toMsg =
  let 
    less = if min == val then noClickButton else
             button [class "image button is-48x48", onClick (toMsg <| val - 1)] [text  "-" ]
    more = if max == val then noClickButton else 
             button [class "image button is-48x48", onClick (toMsg <| val + 1)] [text  "+ "]
  in 
  div [class "level"]
    [ less
    , label [] [text name, b [] [" " ++ String.fromFloat val |> text]]
    , more ]





editTexture : T.SynthPreset -> Html U.UpdateMsg
editTexture preset =
  let 
    textD = D.textureLabel T.Density
    textC = D.textureLabel T.Complexity
  in 
  div [class "is-two-thirds"]
    [ editBoundInt (Tuple.second textD) (Tuple.first textD) D.rangeDensity preset.density U.UpdateDensity 
    , editBoundInt (Tuple.second textC) (Tuple.first textC) D.rangeComplexity preset.complexity U.UpdateComplexity
    ]


buttonOpt : T.SynthRole -> msg -> Html msg
buttonOpt role msg =
  button [] [text (Tuple.first (D.roleLabel role))]


saveButton : T.SynthPreset -> Html U.UpdateMsg
saveButton synth =
  button [class "button", onClick (U.AddPreset synth)] [text "Save"]


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
    [ button [onClick (U.ChangeSelection Nothing), class "delete is-large"] []
    , div [class "column media is-centered p-3"]
      [ figure [class "image mx-auto is-96x96"] [roleThumb preset.role]]
      , synthDescription preset 
      , rolePicker (U.UpdateSynthRole preset) 
      , texEdit preset
      , crudButtons preset ]


editPreset : T.SynthPreset -> Html U.UpdateMsg
editPreset preset =
  div [class "column card  is-half px-6 py-4" ,style  "background" (D.roleColor preset.role)]
    [ synthCardContent preset]


changePresetButton : T.SynthPreset -> Html U.UpdateMsg
changePresetButton preset = 
  div [ onClick (U.ChangeSelection (Just preset))
      , class "my-5" ] 
    [ presetIcon preset.role ]


carouselItem : Float -> Int -> Html msg -> Html msg
carouselItem dTheta i el =
  let 
    degree = (toFloat i) * 15
    rotation = "rotate3d(0, 1, 0," ++ (String.fromFloat degree) ++ "deg)"
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
    dTheta = (modulateInt time) + 0.0 / (toFloat (List.length children))
    yy  = Debug.log "theta" dTheta
  in
  div [class "columns"]
    (List.indexedMap (\i el -> carouselItem dTheta i el) children)
    -- (List.indexedMap (\i el -> spiralItem time dTheta i el) children)


kitRow : (Maybe T.SynthPreset) -> List T.SynthPreset -> Html U.UpdateMsg
kitRow curr presets =
  div [class "column level"] 
    <| [ h5 [class "title" ] [ text "Instruments" ] ]
    ++ List.map changePresetButton presets


testCarousel : Int -> Html U.UpdateMsg
testCarousel time =
  let 
    items = (List.map roleThumb D.roles)
    moreItems = items ++ items ++ items
  in 
  carousel 0 items


synthEditor : T.SynthState -> Html U.UpdateMsg
synthEditor model =
  let 
    yy = Debug.log "model : " model
  in 
  case model.current of 
    Nothing -> 
      div [class "columns"] 
        [ kitRow model.current model.presets
        , button [ onClick U.RequestPreset
                 , class "button" ] [text "Create a Preset"]
         ]
    -- [testCarousel model.time]
    
    Just preset ->
      div [class "columns is-centered"] 
        [ kitRow model.current model.presets 
        , editPreset preset ]


csv : List String -> String
csv strs = 
  let 
    s = List.foldl (\str all -> all ++ str ++ "," )  "" strs 
  in
  String.slice 0 ((String.length s) - 1) s


keys : Html msg
keys =
  -- div [] (List.map (\x -> text (String.fromFloat x)) D.chromaticRoots)
  div [] [ text <| csv  (List.map String.fromFloat D.chromaticRoots)]


backgroundGradient : List String -> Attribute msg
backgroundGradient colors = 
  style "background" 
    <| "linear-gradient(" ++ (csv colors) ++ ")"


kitItem : T.SynthPreset -> Html msg
kitItem preset =
  div [ class "is-hidden-mobile column is-half is-centered has-text-centered" ]
    [ h5 [ class "subtitle has-text-black"] [ text preset.title ]
    , span [style "filter" "invert(1)"] [ roleIcon preset.role ] ]


presetRow : T.NPresetKit -> Html U.UpdateMsg
presetRow (name, kit) =
  div [ onClick (U.ChangePreset kit)
      , class "is-clickable my-3 box column columns is-centered is-multiline is-one-quarter "
      , backgroundGradient ["cyan", "magenta"]] 
     [ h3 [class "column is-full title has-text-centered"] [text name] 
      , div [ class "columns column is-centered is-multiline is-mobile" ] <| List.map kitItem kit ]



presetMenu : List T.NPresetKit -> Html U.UpdateMsg
presetMenu kits =
  div [] <|
    [ h5 [class "title" ] [ text "Presets" ] 
    , div [ class "columns is-multiline is-mobile mx-auto is-centered is-flex is-justify-content-space-around py-3"
      ] <| List.map presetRow kits
    ]


tempoMessage : Float -> Html msg
tempoMessage  cps =
  div [class "content"] 
    [ p [] [text "The speed for you track."]
    , text <| (String.fromFloat (60.0 * cps)) ++ " Beats Per Minute" ]


sizeText : Int -> Int -> String
sizeText cpc size =
  String.fromInt <| cpc * (2^size) 


compoContent : T.Compo -> Html U.EditScore
compoContent model = 
  div [ class "columns card-content has-background-white is-multiline" ]
    -- [ button [onClick (U.ChangeSelection Nothing), class "delete is-large"] []
    [ editBoundInt2 "Meter" "cpc" D.rangeCPC model.cpc U.UpdateCPC
    , editBoundInt2 "Size" (sizeText model.cpc model.size) D.rangeCompoSize model.size U.UpdateSize
    , editRange "Tempo" (tempoMessage model.cps) D.rangeCPS D.rangeStep model.cps U.UpdateCPS 
    , keyPicker model.root U.UpdateRoot ]
      -- [ figure [class "image mx-auto is-96x96"] [roleThumb preset.role]]
      -- , synthDescription preset 
      -- , rolePicker (U.UpdateSynthRole preset) 
      -- , texEdit preset
      -- , crudButtons preset ]


layoutPreview : T.Score -> Html U.EditScore
layoutPreview score = 
  div [] []


keyButton : String -> Int -> (Int -> U.EditScore) -> Int -> Html U.EditScore
keyButton name curr toMsg val =
  button [ onClick (toMsg val)
         , class <| if curr == val then "is-success is-selected" else ""
         , class "button" ] [text name]


keyPicker : Int -> (Int -> U.EditScore) -> Html U.EditScore
keyPicker val toMsg =
  div [class "m-2 box"] 
    <| [ h5 [class "subtitle"] [text "Key"] ]
    ++ List.map (\(v, name) -> keyButton name val toMsg v) D.indexedSharps


editCompo : (Maybe T.Compo) -> Html U.EditScore
editCompo mcompo =
  case mcompo of 
    Nothing ->
      text ""

    Just compo ->
     div [ class "column card  is-half px-6 py-4"
         , style  "background" "cyan" ]
       [ compoContent compo]


editScore : T.EditScore -> Html U.EditScore 
editScore model = 
  div [ class "container" ]
    [ h1 [] [text "Compo Designer"]
    , editCompo model.current
    , layoutPreview model.presets
    , keys ]

main =
  Html.text ""
