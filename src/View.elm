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


reduceScopes : List T.Scope -> T.Scope
reduceScopes  scopes =
  case scopes of 
    [] ->
      D.emptyScope
 
    (first :: rest) ->
      List.foldl(\next prev -> prev) first rest


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


ensembleIcon : T.NamedEnsemble -> Html msg
ensembleIcon (name, ensemble) = 
  div [ class "box" ] 
    [ label [ class "label" ] [ text name ]
    , Components.svg "ensemble"
    , p [ class "content" ] [ text <| (String.fromInt <| List.length ensemble) ++ " voices" ]
    ] 
  

layoutIcon : T.Layout -> Html msg
layoutIcon (name, scopes) = 
  div [ class "box" ] 
    [ label [ class "label" ] [ text name ]
    , Components.svg "layout"
    , p [ class "content" ] [ text <| (String.fromInt <| List.length scopes) ++ " layouts" ]
    ] 
  


roleThumb : T.SynthRole -> Html msg
roleThumb role =
  div [class "p-3 box", style "background" <| D.roleColor role]
    [ img [ style "filter" "invert(1)"
           , width 250
           , height 250
           , src <| "/svg/" ++ (Tuple.first <| D.roleLabel role) ++ ".svg"] [] ]


roleRow : Html msg
roleRow = 
  div [] (List.map roleIcon D.roles)


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
complexityMessage {complexity} =
  if complexity < 2 then 
    text "Very basic harmonics."
  else if complexity < 5 then 
    text "Compelling harmonic motion without getting too far out."
  else
    text "Higher depths of harmony with ambiguous results."


densityMessage : T.Voice -> Html msg
densityMessage {density} =
  if density < 2 then 
    text "Very basic structure."
  else if density < 5 then 
    text "A good amount of structural variation for interest and stability."
  else
    text "A lot of motion, sometimes causing blurriness or obscurity."




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
    options = List.map (\r -> (r, roleIcon r)) D.roles
  in
  div [ class "container box" ] 
    [ h1 [ class "title" ] [ text "Voice Editor" ]
    , Components.editToggle "Duty" (T.Structure, "Structural") (T.Expression, "Expressive") voice.duty updateDuty
    , Components.editText "Label" (text "") voice.label updateLabel
    , Components.editInt "Density" (densityMessage voice) D.rangeDensity voice.density updateDensity
    , Components.editInt "Complexity" (complexityMessage voice) D.rangeComplexity voice.complexity updateComplexity
    , Components.editSelection "Role" (text <| D.roleDescription voice.role) options voice.role updateRole 
    ]


scopePicker : List T.Scope -> (Int -> msg) -> Html msg
scopePicker scopes select =
  div [ class "columns is-multiline level is-vcentered" ] <|
     List.indexedMap (\i scope ->
       div [ class "column is-vcentered has-text-centered", onClick (select i) ]
         [ scopeIcon scope ]) scopes


scopeEditor : List T.Scope -> (Maybe T.Scope) -> (Int -> msg) -> ((Maybe T.Scope) -> msg) -> Html msg
scopeEditor scopes scope select update =
  case scope of
    Nothing ->
      scopePicker scopes select
 
    Just s ->
      editScope s update


voicePicker : List T.Voice -> (Int -> msg) -> Html msg
voicePicker voices select =
  div [ class "columns is-multiline level is-vcentered" ] <|
     List.indexedMap (\i voice ->
       div [ class "column is-vcentered has-text-centered", onClick (select i) ]
         [ voiceIcon voice ]) voices


voiceEditor : List T.Voice -> (Maybe T.Voice) -> (Int -> msg) ->  ((Maybe T.Voice) -> msg) -> Html msg
voiceEditor voices voice select update =
  case voice of  
    Nothing ->
      voicePicker voices select 
 
    Just v ->
      editVoice v update


instrumentEditor : T.Voice -> msg -> msg -> Html msg
instrumentEditor synth keep kill =
  div [class "column card is-half px-6 py-4", style "background" (D.roleColor synth.role)]
    []


view1 = 
  ol [] 
    [ li [] [text "functional designer: Ensemble"]
    , li [] [text "view the thing being edited" ]
    ]


csv : List String -> String
csv strs = 
  let 
    s = List.foldl (\str all -> all ++ str ++ "," )  "" strs 
  in
  String.slice 0 ((String.length s) - 1) s


keys : Html msg
keys =
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


scopeTimeString : T.Scope -> String
scopeTimeString ({cpc, cps, size} as scope) =
  timeString <| duration cpc cps size


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


editScope : T.Scope -> ((Maybe T.Scope) -> msg) -> Html msg
editScope model sig = 
  let
    justSig = (\x -> sig (Just x))
    updateLabel = (\str -> justSig { model | label = str })
    updateCPC = (\int -> justSig { model | cpc = int })
    updateSize = (\int -> justSig { model | size = int })
    updateCPS = (\flt -> justSig { model | cps = flt })
    updateRoot = (\flt -> justSig { model | root = flt })
    -- done = (sig Nothing)
  in 
  div [ class "scope-editor v3" ]
   [ Components.editText "Label" labelInfo model.label updateLabel
   , Components.editInt "Meter" (meterMessage model) D.rangeCPC model.cpc updateCPC
   , Components.editInt "Size" (sizeMessage model) D.rangeScopeSize model.size updateSize 
   , Components.editRange "Tempo" (tempoMessage model) D.rangeCPS model.cps updateCPS 
   , Components.keyPicker False model.root updateRoot 
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


viewEnsembleWithRemover : T.NamedEnsemble -> (T.Voice -> msg) -> Html msg
viewEnsembleWithRemover ensemble remove =
  div [] <|
    [ h3 [ class "subtitle" ] [ text <| "Viewing Ensemble " ++ Tuple.first ensemble]
    ] ++ List.map (\voice -> div [class "box", onClick (remove voice)] [ roleIcon voice.role, span [class "delete"] []]) (Tuple.second ensemble)


ensembleAdder : List T.Voice -> (T.Voice -> msg) -> Html msg
ensembleAdder opts add =
  div [] <|
    [ p [] [ text "Add a voice to this ensemble" ] ]
    ++ List.map (\voice -> div [class "box", onClick (add voice)] [ text voice.label, roleIcon voice.role ] ) opts


ensembleNamer : T.NamedEnsemble -> (String -> msg) -> Html msg
ensembleNamer curr rename =
  Components.editText "Label" (text "") (Tuple.first curr) rename 



thingPicker : List a -> (a -> Html msg) -> (Int -> msg) -> Html msg
thingPicker things icon select = 
  div [ class "columns is-multiline level is-vcentered" ] <|
     List.indexedMap (\i thing ->
       div [ class "column is-vcentered has-text-centered", onClick (select i) ]
         [ icon thing ]) things


layoutPicker : List T.Layout -> (Int -> msg) -> Html msg
layoutPicker layouts select = 
  thingPicker layouts layoutIcon select


ensemblePicker : List T.NamedEnsemble -> (Int -> msg) -> Html msg
ensemblePicker ensembles select =
  div [ class "columns is-multiline level is-vcentered" ] <|
     List.indexedMap (\i ensemble ->
       div [ class "column is-vcentered has-text-centered", onClick (select i) ]
         [ ensembleIcon ensemble ]) ensembles


ensembleEditor : List T.Voice -> List T.NamedEnsemble -> (Maybe T.NamedEnsemble) -> (Int -> msg) -> ((Maybe T.NamedEnsemble) -> msg) -> (List T.NamedEnsemble -> msg) -> Html msg
ensembleEditor options ensembles current select updateCurrent updateAll =
  case current of 
    Nothing ->
      div [] <|
        [ div [] [ text "Choose one of your ensembles here to make changes to it." ]
        , ensemblePicker ensembles select ]

    Just e ->
      let 
        swapEns = (\ee -> Just ((Tuple.first e), ee))
      in 
      div [] <|
        [ viewEnsembleWithRemover e (\voice -> updateCurrent (swapEns (Tools.remove (Just voice) (Tuple.second e))))
        , div [] [ text "Add or remove voices from this ensemble." ] 
        , ensemblePicker ensembles select 
        , ensembleNamer e (\str -> updateCurrent (Just (str, (Tuple.second e)))) 
        , ensembleAdder options (\voice -> updateCurrent (swapEns (Tools.conj voice (Tuple.second e)))) 
        ]


scopeIcon : T.Scope -> Html msg
scopeIcon scope =
  div [ class "box" ] 
    [ label [ class "label" ] [ text scope.label ]
    , Components.svg "scope"
    , p [ class "content" ] [ text (scopeTimeString scope) ]
    ] 


voiceIcon : T.Voice -> Html msg
voiceIcon voice = 
  div [ class "box" ] 
    [ label [ class "label" ] [ text voice.label ]
    , Components.svg "voice"
    , p [ class "content" ] [ text (Tuple.first <| D.roleLabel voice.role) ]
    ] 

  
viewLayoutWithRemover : T.Layout -> (T.Scope -> msg) -> Html msg
viewLayoutWithRemover ((label, scopes) as layout) remove =
  div [] <|
    [ h3 [ class "subtitle" ] [ text <| "Ensemble " ++ label] ] 
    ++ List.map (\scope -> 
           div [class "box"] 
             [ Components.deleteIcon (remove scope)
             , scopeIcon scope ]) scopes


layoutAdder : List T.Scope -> (T.Scope -> msg) -> Html msg
layoutAdder opts add =
  div [] <|
    [ p [] [ text "Add a voice to this layout" ] ]
    ++ List.map (\scope -> div [class "box", onClick (add scope)] [ scopeIcon scope ] ) opts


layoutNamer : String -> (String -> msg) -> Html msg
layoutNamer title rename =
  let 
    content = "A nickname on this scope of music."
  in 
  Components.editText "Label" (text content) title rename 


-- layoutPicker :  (List T.Layout) -> (Int -> msg) -> Html msg
-- layoutPicker layouts select =
  -- div [] <| 
    -- List.indexedMap (\i (label, scopes) -> div [onClick (select i)] [ text label ]) layouts 


layoutEditor : List T.Scope -> List T.Layout -> (Maybe T.Layout) -> (Int -> msg) -> ((Maybe T.Layout) -> msg) -> (List T.Layout -> msg) -> Html msg
layoutEditor options layouts current select updateCurrent updateAll =
  case current of 
    Nothing ->
      div [] <|
        [ div [] [ text "Choose one of your layouts here to make changes to it." ]
        , layoutPicker layouts select
        ] 

    Just ((label, scopes) as l) ->
      div [] <|
        [ viewLayoutWithRemover l (\layout -> updateCurrent (Just (label, (Tools.remove (Just layout) scopes))))
        , div [] [ text "Add or remove scopes from this layout." ] 
        , layoutPicker layouts select
        , layoutNamer label (\str -> updateCurrent (Just (str, scopes)))
        , layoutAdder options (\scope -> updateCurrent (Just (label, Tools.conj scope scopes)))
        ]


main =
  text ""
































lalala = (text "abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabc ") 
