module View exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)

import Components

import Array
import Types as T
import Data as D
import Update as U
import String exposing (String(..))


useSharps = False



invertColor : Html msg -> Html msg
invertColor el =
  div [ style "filter" "invert(1)" ] [ el ] 


keyNames =
  if useSharps then D.sharps else D.flats


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
  div [ class "box"
      , style "background" (D.roleColor role)]
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
  div [class "column columns is-centered is-one-third mb-0 p-0"]
   [ div [class "box p-1 m-3"]
     [ div [class "image is-48x48", style "box-shadow" ("0px 0px 13px " ++ D.roleColor role)]
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


ensemblePreviewIcon : String -> T.SynthRole -> Html msg
ensemblePreviewIcon title role =
  div [ class "ensemble-preview-icon column" ]
     [ div [ class "is-radiusless box is-block p-3 is-flex is-justify-content-space-around is-align-items-center"
           , style "background" <| D.roleColor role ]
        [ invertColor <| roleIcon role, label [ class "is-size-4 has-text-black" ] [ text title ]  ] ]


ensembleThumbIcon : String -> T.SynthRole -> Html msg
ensembleThumbIcon title role =
  div [ class "column box m-3" , style "background" <| D.roleColor role ]
    [ div [ style "text-align" "center", style "filter" "invert(1)" ] [ roleIcon role] ] 


ensemblePreview : T.Ensemble -> Html msg
ensemblePreview ensemble =
  div [class "mt-3 is-multiline"]
    <| List.map (\synth ->  ensemblePreviewIcon (Tuple.first (D.roleLabel <| .role synth)) (.role synth) ) ensemble


ensembleThumbs : T.Ensemble -> Html msg
ensembleThumbs ensemble =
  div [class "box m-3 columns"]
    <| List.map (\synth ->  ensembleThumbIcon "" (.role synth) ) ensemble


ensembleThumbClick : T.Ensemble -> U.EditScore -> Html U.EditScore
ensembleThumbClick ensemble toMsg=
  div [class "box columns my-3",  onClick toMsg]
    <| List.map (\synth ->  ensembleThumbIcon "" (.role synth) ) ensemble



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


editTexture : T.Voice -> Html U.UpdateMsg
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


synthDescription : T.Voice -> Html msg
synthDescription preset =
  div [class "column media-left"]
    [ div [class "media-content"]
    [ p [class "title is-4"] [text preset.label]
    , p [class "content is-6"] [text <| D.roleDescription preset.role] ] ]


texEdit preset =
  div [class "texture-editor column is-two-thirds mx-auto is-centered"] 
    [ editTexture preset ] 


synthCardContent : T.Voice -> Html U.UpdateMsg
synthCardContent preset = 
  let 
    saveSynth = (U.AddPreset preset)
    killSynth = (U.KillPreset preset)
  in 
  div [class "column card-content has-background-white "]
    [ button [onClick (U.ChangeSelection Nothing), class "delete is-large"] []
    , div [class "column media is-centered p-3"]
        <| [ figure [class "image mx-auto is-96x96"] [roleThumb preset.role]
           , synthDescription preset 
           , rolePicker (U.UpdateSynthRole preset) 
           , texEdit preset
           , (Components.skButtons saveSynth killSynth)
           ] ]


hisIsForEditingInstruments : T.Voice -> List (Html msg) -> msg -> (Int -> msg) -> msg -> msg -> Html msg
hisIsForEditingInstruments preset editors selectNone select update delete = 
  div [class "column card-content has-background-white "]
    [ button [onClick selectNone, class "delete is-large"] []
    , div [class "column media is-centered p-3"]
        <| ([ figure [class "image mx-auto is-96x96"] [roleThumb preset.role]
           , synthDescription preset
           , Components.picker [text "one"] select
           ] ++ editors ++ [ (Components.skButtons update delete) ])  ]


complexityMessage : T.Voice -> Html msg
complexityMessage voice =
  text ""


densityMessage : T.Voice -> Html msg
densityMessage voice =
  text ""


voiceEditor : T.Voice -> Html U.EditVoice
voiceEditor voice =
  let 
    updateComplexity = (\n -> U.UVoice { voice | complexity = n })
    updateDensity = (\n -> U.UVoice { voice | density = n })
    uLabel = (\s -> U.UVoice { voice | label = s })

    complexity = Components.editInt "Complexity" (complexityMessage voice) D.rangeCompoSize voice.complexity updateComplexity
    density = Components.editInt "Density" (densityMessage voice) D.rangeCompoSize voice.density updateDensity

    roleOptions = List.map roleIcon D.roles
    updateRole = (\i -> 
      case Array.get i <| Array.fromList D.roles of 
        Just r ->
          U.UVoice { voice | role = r }

        _ ->
          U.UVoice voice)

    editors = [ complexity, density ] 
  in
  div [] 
    [ h1 [ class "title" ] [ text "Voice Editor" ]
    , h1 [ class "title" ] [ text <| D.roleDescription voice.role ]
    , Components.editText "Label" (text "") voice.label uLabel
    , density
    , complexity
    , Components.picker roleOptions updateRole
    ]


viewVoiceEditor : T.VoiceEditor -> Html U.EditVoice
viewVoiceEditor model =
  case model.current of 
    Nothing -> 
      div [] 
        [ h1 [ class "title" ] [ text "Voice Editor, No Voices" ] ]


    Just voice ->
      div [] 
        [ h1 [ class "title" ] [ text "Voice Editor" ]
        , voiceEditor voice ]


editPreset : T.Voice -> Html U.UpdateMsg
editPreset preset =
  div [class "column card is-half px-6 py-4", style "background" (D.roleColor preset.role)]
    [ synthCardContent preset ]


instrumentEditor : T.Voice -> msg -> msg -> Html msg
instrumentEditor synth keep kill =
  div [class "column card is-half px-6 py-4", style "background" (D.roleColor synth.role)]
    []


changePresetButton : T.Voice -> Html U.UpdateMsg
changePresetButton preset = 
  div [ onClick (U.ChangeSelection (Just preset))
      , class "my-5" ] 
    [ presetIcon preset.role ]


kitCol : (Maybe T.Voice) -> List T.Voice -> Html U.UpdateMsg
kitCol curr presets =
  div [class "column level"] 
    <| [ h5 [class "title" ] [ text "Ensemble Designer" ] ]
    ++ List.map changePresetButton presets


synthEditor : (Maybe T.Voice) -> Html U.UpdateMsg
synthEditor preset =
  case preset of 
    Nothing ->
      button [ onClick U.RequestPreset
             , class "button" ] [text "Create a Preset"]

    Just synth ->
      editPreset synth


ensembleEditor : T.VoiceEditor -> Html U.UpdateMsg
ensembleEditor model =
  div [class "columns"]
    [ kitCol  model.current model.presets
    , synthEditor model.current ]


editEnsemble : T.VoiceEditor -> Html U.UpdateMsg
editEnsemble  =
  ensembleEditor


view1 = 
  ol [] 
    [ li [] [text "functional designer: Ensemble"]
    , li [] [text "view the thing being edited" ]
    ] 


edit1Ensemble : T.Ensemble -> Html msg
edit1Ensemble ens =
 div [] <| List.map synthDescription ens


ensembleEditorNew : T.EnsembleEditor -> Html U.EditEnsemble
ensembleEditorNew model =
  case model.current of 
    Just ensemble -> 
      Components.designer view1 edit1Ensemble ensemble

    Nothing ->
      Html.text "no ens"


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


kitItem : T.Voice -> Html msg
kitItem preset =
  div [ class "is-hidden-mobile column is-half is-centered has-text-centered" ]
    [ h5 [ class "subtitle has-text-black"] [ text preset.label ]
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


sectionToLength : Float -> Int -> Float
sectionToLength nCycles cps =
  nCycles * (toFloat cps)


tempoMessage : T.Compo -> Html msg
tempoMessage ({cps, size} as model) =
  div [class "content"] 
    [ p [] [text "The speed for you track. Higher BPM are faster, and lower BPM are slower."]
    , p [] [ text <| "With size " ++ (String.fromInt size) ++ " at " ++ (bpmString cps) ++ "BPM, that means this section is " ++ (String.fromFloat (duration model.cpc model.cps model.size)) ++  " seconds long." ] ]



meterMessage : T.Compo -> Html msg
meterMessage ({cpc} as model) =
  p [] [ text <| "This section has " ++ (timeSigString cpc) ++ " time." ]


sizeToCycles: Int -> Int -> Int
sizeToCycles cpc size =
 cpc * (2^size)


duration : Int -> Float -> Int -> Float
duration cpc cps size  =
  cps * (toFloat (sizeToCycles cpc size))


-- Given a Float seconds, returns a MM:SS string
timeString : Float -> String
timeString t =
  let
    m = ((round t) // 60)
    pad = if m < 10 then "0" else "" 
    mm = pad ++ String.fromInt m
    
    s = modBy 60 (round t)
    pad2 = if s < 10 then "0" else "" 
    ss = pad2 ++ String.fromInt s
  in
  mm ++ ":" ++ ss



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


compoContentMobile : T.Compo -> Html U.EditLayout
compoContentMobile model =
  div [ class "content-mobile columns card-content is-multiline" ]
    [ div [class "column"] 
        [ div [class "column"] [ Components.editText "Label" labelInfo model.label U.UpdateLabel ]
        , editBoundInt3 "Meter" (meterMessage model) D.rangeCPC model.cpc U.UpdateCPC
        , editBoundInt3 "Size" (sizeMessage model) D.rangeCompoSize model.size U.UpdateSize ]
    , div [class "column"] [ editRange "Tempo" (tempoMessage model) D.rangeCPS D.rangeStep model.cps U.UpdateCPS ]
    , div [class "column"] [ keyPicker model.root U.UpdateRoot ] ]

compoContentDesktop : T.Compo -> Html U.EditLayout
compoContentDesktop model =
  div [ class "content-desktop columns card-content is-multiline" ]
    [ Components.editText "Label" labelInfo model.label U.UpdateLabel
    , editBoundInt3 "Size" (sizeMessage model) D.rangeCompoSize model.size U.UpdateSize
    , editBoundInt3 "Meter" (meterMessage model) D.rangeCPC model.cpc U.UpdateCPC
    , editRange "Tempo" (tempoMessage model) D.rangeCPS D.rangeStep model.cps U.UpdateCPS 
    , keyPicker model.root U.UpdateRoot ]


compoContent : T.Compo -> Html U.EditLayout
compoContent model = 
  div []
  [ div [class "is-hidden-desktop is-block-mobile"] [ compoContentMobile model ]
  , div [class "is-hidden-mobile is-block-desktop"] [ compoContentDesktop model ] 
  ]


eeeee : String -> (String -> msg) -> Int -> (Int -> msg) -> Int -> (Int -> msg) -> Html msg
eeeee model toMsg intVal1 intMsg intVal2 intMsg2 = 
  div [ class "content-desktop columns card-content is-multiline" ]
    [ div [class "column"] 
        [ div [class "column"]
          [ Components.editText "Label" labelInfo model toMsg 
          , Components.editInt "Meter" (text "") D.rangeCPC intVal1 intMsg
          , Components.editInt "Size" (text "") D.rangeCompoSize intVal2 intMsg2 ] ] ]


instance : Html U.EditLayout
instance = div [] [ eeeee "test-data" U.UpdateLabel 4 U.UpdateCPC 3 U.UpdateSize ]


layoutItem : T.Compo -> U.EditLayout -> Html U.EditLayout
layoutItem ({cps, cpc, size, root} as model) msg =
  div [class "m-3 column box is-flex is-flex-direction-column", onClick msg]
    [ label [class "subtitle has-text-centered"] [ text model.label ]
    , div [] 
      [ label [] [text "Size "] 
      , b [] [text <| String.fromInt size] ]
    , div [] 
      [ label [] [text  "Tempo "]
      , b [] [text <| String.fromFloat (cpsToBPM cps)] ]
    , div [] 
      [ label [] [text  "Meter"]
      , b [] [text <| String.fromInt cpc] ]
    , div [] 
      [ label [] [text  "Key"]
      , b [] [text <| keyToLabel root] ]
    , div [] 
      [ label [] [text  "Duration"]
      , b [] [text <| timeString <| duration cpc cps size] ]
    ]

editScoreItem : T.Compo -> Html U.EditScore
editScoreItem ({cps, cpc, size, root} as model) =
  div [class "m-3 column box is-flex is-flex-direction-column" ]
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
      , b [] [text <| timeString <| duration cpc cps size] ]
    ]


layoutPreview : List T.Compo -> Html U.EditLayout
layoutPreview score = 
  div [class "box", style "background" "magenta" ] 
    [ div [class "columns"]
      <| List.map (\c -> layoutItem c (U.ChangeLayoutSelection (Just c))) score ]


designPreview : List T.Compo -> Html U.EditLayout
designPreview score = 
  div [class "box", style "background" "magenta" ] 
    [ div [class "columns is-multiline"]
      <| List.indexedMap (\i c -> layoutItem c (U.RemoveLayoutAt i)) score ]


designOptions : List T.Compo -> Html U.EditLayout
designOptions score = 
  div [class "box", style "background" "orange" ] 
    [ div [class "columns"]
      <| List.indexedMap (\i c -> layoutItem c (U.AddLayoutSection c)) score ]


layoutOptions : List T.Compo -> Html U.EditLayout
layoutOptions opts = 
  div [class "box", style "background" "orange" ] 
    [ div [class "columns"]
      <| List.map (\c -> layoutItem c (U.ChangeLayoutSelection <| Just c)) opts ]


keyButtonMobile : String -> Int -> (Int -> U.EditLayout) -> Int -> Html U.EditLayout
keyButtonMobile name curr toMsg val =
  button [ onClick (toMsg val)
         , class <| if curr == val then "is-success is-selected" else ""
         , class "column p-0 is-4 button" ] [text name]


keyButtonDesktop : String -> Int -> (Int -> U.EditLayout) -> Int -> Html U.EditLayout
keyButtonDesktop name curr toMsg val =
  button [ onClick (toMsg val)
         , class <| if curr == val then "is-success is-selected" else ""
         , class "column is-3 button p-0" ] [text name]


keyPickerMobile : Int -> (Int -> U.EditLayout) -> Html U.EditLayout
keyPickerMobile val toMsg =
  div [class "key-picker-mobile"] 
    <| [ h5 [class "subtitle"] [text "Key"] ]
    ++ [ div [class "columns is-multiline is-mobile"] 
       <| List.map (\(v, name) -> keyButtonDesktop name val toMsg v) (if useSharps then D.indexedSharps else D.indexedFlats) ]


keyPickerDesktop : Int -> (Int -> U.EditLayout) -> Html U.EditLayout
keyPickerDesktop val toMsg =
  div [class ""] 
    <| [ h5 [class "subtitle"] [text "Key"] ] 
    ++ [ div [class "columns is-multiline"] 
       <| List.map (\(v, name) -> keyButtonDesktop name val toMsg v) (if useSharps then D.indexedSharps else D.indexedFlats) ]


keyPicker : Int -> (Int -> U.EditLayout) -> Html U.EditLayout
keyPicker val toMsg =
  div [class "m-3 box container"] 
    [ div [class "is-hidden-tablet"] [keyPickerMobile val toMsg]
    , div [class "is-hidden-mobile"] [keyPickerDesktop val toMsg]
    ]


killCompoButton : T.Compo -> Html U.EditLayout
killCompoButton model =
  button [ onClick (U.KillCompo model), class "button" ] [ text "Delete Section" ]


editCompo : (Maybe T.Compo) -> Html U.EditLayout
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


editLayoutMessage : Html msg
editLayoutMessage = 
  p [class "content"] [text "Use this editor to create sections for your songs."]


designLayoutMessage : Html msg
designLayoutMessage = 
  p [class "content"] [text "Arrange your score here." ]


editLayout : T.EditLayout -> Html U.EditLayout 
editLayout model = 
  div [ class "container" ]
    [ h1 [ class "title" ] [ text "Layout Editor"]
    , p [ class ""] [ editLayoutMessage]
    , div [ class "columns is-multiline"] 
      [ div [ class "column is-full" ] [ editCompo model.current ]
      , div [ class "column is-full" ] [ layoutOptions model.presets ] ] ]


totalLength : List T.Compo -> Float
totalLength score =
  List.foldl (\{cpc, cps, size} sum -> 
    sum + (duration cpc cps size)) 0 score


countStems : List T.Section -> Int
countStems score =
  List.foldl (\(compo, ensemble) sum ->  
    sum + (List.length ensemble)) 0 score


designLayout : T.EditLayout -> Html U.EditLayout 
designLayout model = 
  let
-- duration cpc cps size  =
    length = totalLength model.list
  in 
  div [ class "container" ]
    [ h1 [ class "title" ] [ text "Layout Designer"]
    , p [ class ""] [ designLayoutMessage]
    , p [] [text <| timeString length ]
    , div [ class "columns is-multiline"] 
      [ div [ class "column is-full" ] [ designPreview model.list ] ]
      , div [ class "column is-full" ] [ designOptions model.presets ] ]


scorePreviewItem : T.Section -> (T.Section -> U.EditScore) -> Html U.EditScore
scorePreviewItem ((compo, ensemble) as section) toMsg =
  div [ class "box" 
      , onClick (toMsg section) ] 
    [ div [ class "columns is-multiline" ]
      [ label [ class "column is-full title has-text-centered" ] [ text compo.label ]
      , div [ class "column is-full"] [ ensemblePreview ensemble ]
      ] ]


scoreData : T.Section -> Html U.EditScore
scoreData ((compo, ensemble) as section) =
  div [ class "box" ]
    [ div [ class "columns is-multiline" ]
      [ label [ class "column is-full title has-text-centered" ] [ text compo.label ]
      , div [ class "column is-full"] [ ensemblePreview ensemble ]
      ] ]


scorePreview : List T.Section -> Html U.EditScore
scorePreview sections = 
  div [ class "box" ] 
    [ h3 [ class "subtitle" ] [ text "Score" ]
    , div [ class "columns" ] <| List.map (\s -> scorePreviewItem s U.ApplySection)  sections ]


scoreView : List T.Section -> Html U.EditScore
scoreView sections = 
  div [ class "box" ] 
    [ h3 [ class "subtitle" ] [ text "Score" ]
    , div [ class "columns" ] <| List.map scoreData  sections ]



makeSection : T.Section -> Html msg
makeSection (mCompo, mEnsemble) =
  div [] [ text "it has some maybethings" ]


compoThumb : T.Compo -> U.EditScore -> Html U.EditScore
compoThumb compo toMsg =
  div [ class "box column m-3"
      , onClick toMsg ] 
    [ p [] [ text compo.label ]
    , p [] [ text (timeString (duration compo.cpc compo.cps compo.size)) ] ]


ensembleBanner : T.Ensemble -> Html msg
ensembleBanner ensemble =
  div [ class "ensemble-banner columns is-multiline pt-3" ] <| List.map (\synth ->
    div [ class "column is-full"] [ roleIcon synth.role ] ) ensemble


sectionThumb : (T.Compo, T.Ensemble) -> Html U.EditScore
sectionThumb (compo, ensemble) =
  div [ style "max-width" "90px"
      , class "m-3 box column has-text-centered" ]
    [ p [] [ text compo.label ]
    , p [] [ text (timeString (duration compo.cpc compo.cps compo.size)) ]
    , ensembleBanner ensemble ]


chooseCompo : List T.Compo -> (T.Compo -> U.EditScore) -> (Maybe T.Compo) -> Html U.EditScore
chooseCompo compos toMsg compo =
  let
    picker = div [ class "columns my-3" ] <| List.map (\c -> compoThumb c (toMsg c)) compos
  in 
  case compo of 
    Nothing -> 
      div [] 
        [ text "You need a compo for this section. Pick one here." 
        , picker
        ]
 
    Just comp -> 
       picker


chooseEnsemble : List T.Ensemble -> (T.Ensemble -> U.EditScore) -> (Maybe T.Ensemble) -> Html U.EditScore
chooseEnsemble ensembles toMsg ensemble =
  let 
    picker = div [ class "" ] <| List.map (\e -> ensembleThumbClick e (toMsg e)) ensembles
  in 
  case ensemble of 
    Nothing -> 
      div [] 
        [ text "You need an ensemble for this section. Pick one here." 
        , picker
        ]

    Just ens -> 
     picker


sectionPreview : T.Compo -> T.Ensemble -> Html U.EditScore
sectionPreview compo ensemble =
  div [ class "columns", style "background" "linear-gradient(90deg, cyan, magenta)" ] 
  [ div [ class "column is-half" ] [ h3 [] [ text "Details" ],  editScoreItem compo ] 
  , div [ class "column is-half" ] [ h3 [] [ text "Ensemble" ],  ensembleThumbs ensemble ]
  ] 


editSection : List T.Compo -> List T.Ensemble ->  (Maybe T.Section) -> Html U.EditScore
editSection cs es mSection = 
  case mSection of 
    Nothing ->
      div [] 
        [ button [ class "button", onClick U.NewSection ] [ text "Add a new section" ]
        , p [] [ text "Or choose from one of the sections below to update it." ] ]
    
    Just (compo, ensemble) ->
      div [] 
        [ sectionPreview compo ensemble
        , div [ class "columns" ]
          [ div [ class "column is-half" ] [ b [] [ text "Change the Details" ],  chooseCompo cs (\x -> U.UpdateSection x ensemble) (Just compo) ]
          , div [ class "column is-half" ] [ b [] [ text "Change the Ensemble" ], chooseEnsemble es (\x -> U.UpdateSection compo x) (Just ensemble) ] ] ]


editScore : T.EditScore -> Html U.EditScore
editScore ({ current, list, layout, ensembles} as model) = 
  div [ class "container" ]
    [ h1 [ class "title" ] [ text "Score Designer"]
    , div [ class "columns is-multiline"] 
        [ div [ class "column is-full" ] [ editSection layout ensembles current] ]
        -- , div [ class "column is-full" ] [ scoreView list ] 
        , div [ class "column is-full" ] [ scorePreview list] ]


overviewScore : List T.Section -> Html U.EditScore
overviewScore score =
  div [] 
    [ div [class "columns"] (List.map sectionThumb score)
    , p [ class "subtitle" ] [ text <| "This song is " ++ (timeString <| totalLength (List.map Tuple.first score)) ++ " in duration."]
    , p [ class "subtitle" ] [ text <| "It takes stems " ++ (String.fromInt <| countStems score ) ++ " to write this song."]
    , button [ class "button" ] [ text "Print Song" ]
    ]


main =
  instance
