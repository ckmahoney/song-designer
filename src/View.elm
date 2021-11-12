module View exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)

import Array
import Types as T
import Data as D
import Update as U
import String exposing (String(..))


useSharps = False


keyNames =
  if useSharps then D.sharps else D.flats



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



editRangeMobile : String -> Html msg -> (Float, Float) -> Float -> Float -> (Float -> msg) -> Html msg
editRangeMobile title html (mn, mx) step_ val toMsg =
  div [class "m-3 box level"
      , onInput (\s -> toMsg <| Maybe.withDefault 0.0 <| String.toFloat s) ]
    [ div [class "columns is-multiline"]
      [ div [class "column is-full level is-flex is-justify-content-space-around"] 
        [ label [class "m-0 subtitle"] [text title]
        , input [ type_ "range"
                , class "m-0"
                , value <| String.fromFloat val
                , step <| String.fromFloat step_
                , Attr.min <| String.fromFloat mn
                , Attr.max <| String.fromFloat mx ] [] ]
      , div [class "column"] [ html ] ] ]


editRangeDesktop : String -> Html msg -> (Float, Float) -> Float -> Float -> (Float -> msg) -> Html msg
editRangeDesktop title html (mn, mx) step_ val toMsg =
  div [class "m-3 box level"
      , onInput (\s -> toMsg <| Maybe.withDefault 0.0 <| String.toFloat s) ]
    [ div [class "columns is-multiline"]
      [ div [class "column is-full level"] 
        [ label [class "m-0 subtitle"] [text title]
        , input [ type_ "range"
                , class "m-0"
                , value <| String.fromFloat val
                , step <| String.fromFloat step_
                , Attr.min <| String.fromFloat mn
                , Attr.max <| String.fromFloat mx ] [] ]
      , div [class "column"] [ html ] ] ]


editRange : String -> Html msg -> (Float, Float) -> Float -> Float -> (Float -> msg) -> Html msg
editRange title html (mn, mx) step_ val toMsg =
  div []
   [ div [class "is-hidden-tablet"] [ editRangeMobile title html (mn, mx) step_ val toMsg ]
   , div [class "is-hidden-mobile"] [ editRangeDesktop title html (mn, mx) step_ val toMsg ] ]

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
  div [class "m-3 column box"]
    [ h3 [class "subtitle"] [text title]
    , div [] [ less
    , label [] [text name, b [] [" " ++ String.fromInt val |> text]]
    , more ] ]


editBoundInt3 : String -> Html msg -> (Int,  Int) -> Int -> (Int -> msg) -> Html msg
editBoundInt3 title html (min, max) val toMsg =
  let 
    less = if min == val then noClickButton else
             button [class "image button is-48x48", onClick (toMsg <| val - 1)] [text  "-" ]
    more = if max == val then noClickButton else 
             button [class "image button is-48x48", onClick (toMsg <| val + 1)] [text  "+ "]
  in 
  div [ class "m-3 box"]
    [ div [ class "columns is-multiline"]
      [ div [ class "columns is-multiline column is-full is-flex is-flex-row is-justify-content-space-between"] 
            [ h5 [ class "column is-full subtitle"] [ text title ]
            , div [ class "mt-0 column is-full is-flex is-flex-row level"] 
                     [ less
                     , b [] [" " ++ String.fromInt val |> text ]
                     , more ] ] 
      , div [ class "column box has-text-light has-background-info is-full"] [ html ] ] ]


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



cpsToBPM : Float -> Float
cpsToBPM cps =
  60.0 * cps


timeSigString : Int -> String
timeSigString cpc =
  (String.fromInt cpc) ++ " / 4"


bpmString cps =
  String.fromFloat <| cpsToBPM cps


tempoMessage : T.Compo -> Html msg
tempoMessage ({cps, size} as model) =
  div [class "content"] 
    [ p [] [text "The speed for you track. Higher BPM are faster, and lower BPM are slower."]
    , p [] [ text <| "With size " ++ (String.fromInt size) ++ " at " ++ (bpmString cps) ++ " that means it will be " ++ " seconds long." ]
    , text <| (String.fromFloat (cpsToBPM cps)) ++ " Beats Per Minute" ]


meterMessage : T.Compo -> Html msg
meterMessage ({cpc} as model) =
  p [] [ text <| "This section has " ++ (timeSigString cpc) ++ " time." ]


sizeToCycles: Int -> Int -> Int
sizeToCycles cpc size =
 cpc * (2^size)


duration : Int -> Float -> Int -> Float
duration cpc cps size  =
  cps * (toFloat (sizeToCycles cpc size))


sizeText : Int -> Int -> String
sizeText cpc size =
  String.fromInt <| sizeToCycles cpc size


sizeMessage : T.Compo -> Html msg
sizeMessage ({size, cpc} as model) =
  div [] 
    [ p [] [ text <| "Using size " ++ (String.fromInt size) ++ " and " ++ String.fromInt cpc ++ " cycles per measure," ]
    , p [] [ text <| "That means this section is " ++ (sizeText cpc size) ++ " cycles long." ] ]


keyToLabel : Int -> String
keyToLabel index =
   Maybe.withDefault "Key Not Found"
     <| Array.get index <| Array.fromList keyNames


labelInfo : Html msg
labelInfo =
  div [class "content"] [ text "A name to help you remember what this section is for." ]


editTextMobile : String -> Html msg -> String -> ( String -> msg ) -> Html msg
editTextMobile title html val toMsg =
  div [class ""]
    [ label [ ] [ text title ]
    , input [ type_ "text"
            , class "input my-3 is-info"
            , value val
            , onInput toMsg ] []
    , html
    ]


editTextDesktop : String -> Html msg -> String -> ( String -> msg ) -> Html msg
editTextDesktop title html val toMsg =
  div [class ""]
    [ label [ ] [ text title ]
    , input [ type_ "text"
            , class "input my-3 is-info"
            , value val
            , onInput toMsg ] []
    , html
    ]


editText : String -> Html msg -> String -> ( String -> msg ) -> Html msg
editText title html val toMsg =
  div [class "m-3 box"]
    [ div [ class "is-hidden-tablet" ] [ editTextMobile title html val toMsg ]
    , div [ class "is-hidden-mobile" ] [ editTextDesktop title html val toMsg ] ] 


compoContentMobile : T.Compo -> Html U.EditScore
compoContentMobile model =
  div [ class "content-mobile columns card-content is-multiline" ]
    [ div [class "column"] 
        [ editBoundInt3 "Meter" (meterMessage model) D.rangeCPC model.cpc U.UpdateCPC
        , editBoundInt3 "Size" (sizeMessage model) D.rangeCompoSize model.size U.UpdateSize ]
    , div [class "column"] [ editText "Label" labelInfo model.label U.UpdateLabel ]
    , div [class "column"] [ editRange "Tempo" (tempoMessage model) D.rangeCPS D.rangeStep model.cps U.UpdateCPS ]
    , div [class "column"] [ keyPicker model.root U.UpdateRoot ] ]


compoContentTablet : T.Compo -> Html U.EditScore
compoContentTablet model =
  div [ class "content-tablet columns card-content is-multiline" ]
    [ div [class "column is-half"] 
        [ editBoundInt3 "Meter" (meterMessage model) D.rangeCPC model.cpc U.UpdateCPC
        , editBoundInt3 "Size" (sizeMessage model) D.rangeCompoSize model.size U.UpdateSize ]
    , div [class "column is-half"] [ editText "Label" labelInfo model.label U.UpdateLabel ]
    , div [class "column is-half"] [ editRange "Tempo" (tempoMessage model) D.rangeCPS D.rangeStep model.cps U.UpdateCPS ]
    , div [class "column is-half"] [ keyPicker model.root U.UpdateRoot ] ]


compoContentDesktop : T.Compo -> Html U.EditScore
compoContentDesktop model =
  div [ class "content-desktop columns card-content is-multiline" ]
    [ editBoundInt3 "Meter" (meterMessage model) D.rangeCPC model.cpc U.UpdateCPC
    , editBoundInt3 "Size" (sizeMessage model) D.rangeCompoSize model.size U.UpdateSize
    , editText "Label" labelInfo model.label U.UpdateLabel
    , editRange "Tempo" (tempoMessage model) D.rangeCPS D.rangeStep model.cps U.UpdateCPS 
    , keyPicker model.root U.UpdateRoot ]


compoContent : T.Compo -> Html U.EditScore
compoContent model = 
  div []
  [ div [class "is-hidden-tablet is-block-mobile"] [ compoContentMobile model ]
  , div [class "is-hidden-mobile is-hidden-desktop is-block-tablet-only"] [ compoContentTablet model ] 
  , div [class "is-hidden-mobile is-hidden-tablet-only is-block-desktop"] [ compoContentDesktop model ] 
  ]

layoutItem : T.Compo -> U.EditScore -> Html U.EditScore
layoutItem ({cps, cpc, size, root} as model) msg =
  div [class "m-3 column box is-flex is-flex-direction-column", onClick msg]
    [ label [class "subtitle has-text-centered"] [ text model.label ]
    , div [] 
      [ label [] [text "Size "] 
      , b [] [text <| String.fromInt size] ]
    , div [] 
      [ label [] [text  "Tempo"]
      , b [] [text <| String.fromFloat (cpsToBPM cps)] ]
    , div [] 
      [ label [] [text  "Meter"]
      , b [] [text <| String.fromInt cpc] ]
    , div [] 
      [ label [] [text  "Key"]
      , b [] [text <| keyToLabel root] ]
    , div [] 
      [ label [] [text  "Duration"]
      , b [] [text <| String.fromFloat <| duration cpc cps size] ]
    ]


layoutPreview : List T.Compo -> Html U.EditScore
layoutPreview score = 
  div [class "box", style "background" "magenta" ] 
    [ div [class "columns"]
      <| List.map (\c -> layoutItem c (U.ChangeScoreSelection (Just c))) score ]


keyButtonMobile : String -> Int -> (Int -> U.EditScore) -> Int -> Html U.EditScore
keyButtonMobile name curr toMsg val =
  button [ onClick (toMsg val)
         , class <| if curr == val then "is-success is-selected" else ""
         , class "column p-0 is-4 button" ] [text name]


keyButtonDesktop : String -> Int -> (Int -> U.EditScore) -> Int -> Html U.EditScore
keyButtonDesktop name curr toMsg val =
  button [ onClick (toMsg val)
         , class <| if curr == val then "is-success is-selected" else ""
         , class "column is-3 button p-0" ] [text name]


keyPickerMobile : Int -> (Int -> U.EditScore) -> Html U.EditScore
keyPickerMobile val toMsg =
  div [class "key-picker-mobile"] 
    <| [ h5 [class "subtitle"] [text "Key"] ]
    ++ [ div [class "columns is-multiline is-mobile"] 
       <| List.map (\(v, name) -> keyButtonDesktop name val toMsg v) (if useSharps then D.indexedSharps else D.indexedFlats) ]


keyPickerDesktop : Int -> (Int -> U.EditScore) -> Html U.EditScore
keyPickerDesktop val toMsg =
  div [class ""] 
    <| [ h5 [class "subtitle"] [text "Key"] ] 
    ++ [ div [class "columns is-multiline"] 
       <| List.map (\(v, name) -> keyButtonDesktop name val toMsg v) (if useSharps then D.indexedSharps else D.indexedFlats) ]


keyPicker : Int -> (Int -> U.EditScore) -> Html U.EditScore
keyPicker val toMsg =
  div [class "m-3 box container"] 
    [ div [class "is-hidden-tablet"] [keyPickerMobile val toMsg]
    , div [class "is-hidden-mobile"] [keyPickerDesktop val toMsg]
    ]


killCompoButton : T.Compo -> Html U.EditScore
killCompoButton model =
  button [ onClick (U.KillCompo model), class "button" ] [ text "Delete Section" ]


editCompo : (Maybe T.Compo) -> Html U.EditScore
editCompo mcompo =
  case mcompo of 
    Nothing ->
      div []
        [ p [ class "mb-3" ] [ text "Select an existing section below, or create a new one." ]

        , button [ onClick U.RequestCompo, class "button" ] [ text "Create a new Section"] ]

    Just compo ->
      div [ class "column card px-6 py-4"
          , style  "background" "cyan" ]
          [ killCompoButton compo
          , compoContent compo]


editScore : T.EditScore -> Html U.EditScore 
editScore model = 
  div [ class "container" ]
    [ h1 [ class "title" ] [ text "Layout Designer"]
    , div [ class "columns is-multiline"] 
      [ div [ class "column is-full" ] [ editCompo model.current ]
      , div [ class "column is-full" ] [ layoutPreview model.presets ] ]
    ]


main =
  Html.text ""
