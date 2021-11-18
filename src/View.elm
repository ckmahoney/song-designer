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
import Tools

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


buttonOpt : T.SynthRole -> msg -> Html msg
buttonOpt role msg =
  button [] [text (Tuple.first (D.roleLabel role))]


synthDescription : T.Voice -> Html msg
synthDescription preset =
  div [class "column media-left"]
    [ div [class "media-content"]
    [ p [class "title is-4"] [text preset.label]
    , p [class "content is-6"] [text <| D.roleDescription preset.role] ] ]


complexityMessage : T.Voice -> Html msg
complexityMessage voice =
  text ""


densityMessage : T.Voice -> Html msg
densityMessage voice =
  text ""


rolePicker2 :(T.SynthRole -> msg) -> Html msg
rolePicker2 select =
  div [] <|
    List.map (\role -> 
      div [onClick (select role)] [roleIcon role]) D.roles


editVoice : T.Voice -> ((Maybe T.Voice) -> msg) -> Html msg
editVoice voice sig =
  let 
    justSig = (\x -> sig (Just x))
    updateComplexity = (\n -> justSig { voice | complexity = n })
    updateDensity = (\n -> justSig { voice | density = n })
    updateLabel = (\s -> justSig { voice | label = s })
    updateRole = (\r -> justSig { voice | role = r } )
    updateDuty = (\d -> justSig { voice | duty = d } )
  in
  div [] 
    [ h1 [ class "title" ] [ text "Voice Editor" ]
    , h1 [ class "title" ] [ text <| D.roleDescription voice.role ]
    , Components.editToggle "Duty" (T.Structure, "Structural") (T.Expression, "Expressive") voice.duty updateDuty
    , Components.editText "Label" (text "") voice.label updateLabel
    , Components.editInt "Density" (densityMessage voice) D.rangeDensity voice.density updateDensity
    , Components.editInt "Complexity" (complexityMessage voice) D.rangeComplexity voice.complexity updateComplexity
    , rolePicker2 updateRole
    ]


scopeEditor :(Maybe T.Scope) -> ((Maybe T.Scope) -> msg) -> Html msg
scopeEditor scope update =
  case scope of
    Nothing ->
      text "No scope right now"
 
    Just s ->
      editScope s update


voiceEditor : (Maybe T.Voice) -> ((Maybe T.Voice) -> msg) -> Html msg
voiceEditor voice update =
  case voice of  
    Nothing ->
      text "No voice right now"
 
    Just v ->
      editVoice v update


instrumentEditor : T.Voice -> msg -> msg -> Html msg
instrumentEditor synth keep kill =
  div [class "column card is-half px-6 py-4", style "background" (D.roleColor synth.role)]
    []


kitCol : (Maybe T.Voice) -> List T.Voice -> Html U.UpdateMsg
kitCol curr presets =
  div [class "column level"] 
    <| [ h5 [class "title" ] [ text "Ensemble Designer" ] ]
    -- ++ List.map changePresetButton presets


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


tempoMessage : T.Scope -> Html msg
tempoMessage ({cps, size} as model) =
  div [class "content"] 
    [ p [] [text "The speed for you track. Higher BPM are faster, and lower BPM are slower."]
    , p [] [ text <| "With size " ++ (String.fromInt size) ++ " at " ++ (bpmString cps) ++ "BPM, that means this section is " ++ (String.fromFloat (duration model.cpc model.cps model.size)) ++  " seconds long." ] ]


meterMessage : T.Scope -> Html msg
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


sizeMessage : T.Scope -> Html msg
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




layoutItem : T.Scope -> U.EditLayout -> Html U.EditLayout
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

editScoreItem : T.Scope -> Html U.EditScore
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


layoutPreview : List T.Scope -> Html U.EditLayout
layoutPreview score = 
  div [class "box", style "background" "magenta" ] 
    [ div [class "columns"]
      <| List.map (\c -> layoutItem c (U.ChangeLayoutSelection (Just c))) score ]


designPreview : List T.Scope -> Html U.EditLayout
designPreview score = 
  div [class "box", style "background" "magenta" ] 
    [ div [class "columns is-multiline"]
      <| List.indexedMap (\i c -> layoutItem c (U.RemoveLayoutAt i)) score ]


designOptions : List T.Scope -> Html U.EditLayout
designOptions score = 
  div [class "box", style "background" "orange" ] 
    [ div [class "columns"]
      <| List.indexedMap (\i c -> layoutItem c (U.AddLayoutSection c)) score ]


layoutOptions : List T.Scope -> Html U.EditLayout
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


killScopeButton : T.Scope -> Html U.EditLayout
killScopeButton model =
  button [ onClick (U.KillScope model), class "button" ] [ text "Delete Section" ]


editLayoutMessage : Html msg
editLayoutMessage = 
  p [class "content"] [text "Use this editor to create sections for your songs."]


designLayoutMessage : Html msg
designLayoutMessage = 
  p [class "content"] [text "Arrange your score here." ]


totalLength : List T.Scope -> Float
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
makeSection (mScope, mEnsemble) =
  div [] [ text "it has some maybethings" ]


compoThumb : T.Scope -> U.EditScore -> Html U.EditScore
compoThumb compo toMsg =
  div [ class "box column m-3"
      , onClick toMsg ] 
    [ p [] [ text compo.label ]
    , p [] [ text (timeString (duration compo.cpc compo.cps compo.size)) ] ]


ensembleBanner : T.Ensemble -> Html msg
ensembleBanner ensemble =
  div [ class "ensemble-banner columns is-multiline pt-3" ] <| List.map (\synth ->
    div [ class "column is-full"] [ roleIcon synth.role ] ) ensemble


sectionThumb : (T.Scope, T.Ensemble) -> Html U.EditScore
sectionThumb (compo, ensemble) =
  div [ style "max-width" "90px"
      , class "m-3 box column has-text-centered" ]
    [ p [] [ text compo.label ]
    , p [] [ text (timeString (duration compo.cpc compo.cps compo.size)) ]
    , ensembleBanner ensemble ]


overviewScore : List T.Section -> Html U.EditScore
overviewScore score =
  div [] 
    [ div [class "columns"] (List.map sectionThumb score)
    , p [ class "subtitle" ] [ text <| "This song is " ++ (timeString <| totalLength (List.map Tuple.first score)) ++ " in duration."]
    , p [ class "subtitle" ] [ text <| "It takes stems " ++ (String.fromInt <| countStems score ) ++ " to write this song."]
    , button [ class "button" ] [ text "Print Song" ]
    ]


editScope : T.Scope -> ((Maybe T.Scope) -> msg) -> Html msg
editScope model sig = 
  let
    justSig = (\x -> sig (Just x))
    updateLabel = (\str -> justSig { model | label = str })
    updateCPC = (\int -> justSig { model | cpc = int })
    updateSize = (\int -> justSig { model | size = int })
    updateCPS = (\flt -> justSig { model | cps = flt })
    updateRoot = (\flt -> justSig { model | root = flt })
    done = (sig Nothing)
  in 
  div [ class "scope-editor v3" ]
   [ Components.editText "Label" labelInfo model.label updateLabel
   , Components.editInt "Meter" (meterMessage model) D.rangeCPC model.cpc updateCPC
   , Components.editInt "Size" (sizeMessage model) D.rangeScopeSize model.size updateSize 
   , Components.editRange "Tempo" (tempoMessage model) D.rangeCPS model.cps updateCPS 
   , Components.keyPicker False model.root updateRoot 
   , button [onClick done] [ text "done" ] 
   ] 


designEnsemble : List T.Voice -> (Maybe (List T.Voice)) -> ((Maybe (List T.Voice)) -> msg) -> Html msg
designEnsemble options current update =
  case current of 
    Nothing ->
      div [] <|
        [ div [] [ text "Select a voice below to begin creating a new ensemble." ] ]
        ++ List.map (\opt -> div [onClick (update (Just [opt]))] [text opt.label ]) options 

    Just ens ->
      div [] [ text "Add and remove voices from this ensemble to update it. When you are done, save your changes." ]


viewEnsemble : T.Ensemble -> Html msg
viewEnsemble ensemble =
  div [] <|
    [ h3 [ class "subtitle" ] [ text "Viewing Ensemble" ]
    ] ++ List.map (\voice -> div [] [ roleIcon voice.role ] ) ensemble


viewEnsembleWithRemover : T.Ensemble -> (T.Voice -> msg) -> Html msg
viewEnsembleWithRemover ensemble remove =
  div [] <|
    [ h3 [ class "subtitle" ] [ text "Viewing Ensemble" ]
    ] ++ List.map (\voice -> div [class "box", onClick (remove voice)] [ roleIcon voice.role, span [class "delete"] []]) ensemble


ensembleAdder : List T.Voice -> T.Ensemble -> (T.Voice -> msg) -> Html msg
ensembleAdder opts curr add =
  div [] <|
    [ p [] [ text "Add a voice to this ensemble" ] ]
    ++ List.map (\voice -> div [class "box", onClick (add voice)] [ text voice.label, roleIcon voice.role ] ) opts


ensembleEditor : List T.Voice -> List T.Ensemble -> (Maybe (List T.Voice)) -> (Int -> msg) -> ((Maybe T.Ensemble) -> msg) -> (List T.Ensemble -> msg) -> Html msg
ensembleEditor options ensembles current chooseEnsemble updateCurrent updateAll =
  let
    done = 
      Components.button (updateCurrent Nothing) [] "Done"
    ensemblePicker = 
      List.indexedMap (\i ens -> div [onClick (chooseEnsemble i)] [ text <| "ensemble " ++ String.fromInt i ]) ensembles 
  in 
  case current of 
    Nothing ->
      div [] <|
        [ div [] [ text "Choose one of your ensembles here to make changes to it." ]
        , done ]
        ++ ensemblePicker

    Just e ->
      div [] <|
        [ viewEnsembleWithRemover e (\voice -> updateCurrent (Just (Tools.remove (Just voice) e)))
        , div [] [ text "Add or remove voices from this ensemble." ] ]
        ++ ensemblePicker
        ++ [ ensembleAdder options e (\voice -> updateCurrent (Just (Tools.conj voice e))) ]
        ++ [ done

        ]



main =
  text ""
