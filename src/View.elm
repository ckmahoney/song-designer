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


keyNames =
  if useSharps then D.sharps else D.flats


roleRow : Html msg
roleRow = 
  div [] (List.map roleIcon D.roles)


roleDescription : T.SynthRole -> Html msg
roleDescription role =
  div [ class "content" ] [ b [] [ text <| Tuple.first <| D.roleLabel role],  p [] [text <| D.roleDescription role ] ]


labelInfo : Html msg
labelInfo =
  div [class "content"] [ text "Descriptive name for this scope of music." ]


editLayoutMessage : Html msg
editLayoutMessage = 
  p [class "content"] [text "Use this editor to create sections for your songs."]


designLayoutMessage : Html msg
designLayoutMessage = 
  p [class "content"] [text "Arrange your score here." ]


reduceScopes : List T.Scope -> T.Scope
reduceScopes  scopes =
  case scopes of 
    [] ->
      D.emptyScope
 
    (first :: rest) ->
      List.foldl(\next prev -> prev) first rest


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
  

asset = 
  Components.card "Assets" <|
    div [ class "columns" ]
      [ div [ class "column is-half" ] 
        []
        -- [ scoreIcon True ]
      , div [ class "is-half" ]
        [ div [class "level"] [ (Components.svg "sheet-music"), text "Download sheet music" ]
        , div [class "level"] [ (Components.svg "midi-logo"), text "Download MIDI files" ]
        , div [class "level"] [ (Components.svg "file-zip"), text "Download Sample Pack/Stems" ]
        , div [class "level"] [ (Components.svg "yin-yang"), text "Create Another Like This" ]
        ] ] 


scoreIcon : T.Score ->  Html msg
scoreIcon (meta, combos) =
  div [ class "container" ] 
    [ label [ class "label" ] [ text meta.title ]
    , Components.svg "score"
    , p [ class "content" ] [ text "all the score details go here once you make a score editor" ]
    ] 


templateIcon : T.Template ->  Html msg
templateIcon (mMeta, mCombos) =
  div [ class "container" ] 
    [ label [ class "label" ] [ text <| Maybe.withDefault "No title" mMeta.title ]
    , Components.svg "score"
    , p [ class "content" ] [ text "This is a template for making scores. Use it as a basis for making variations of styles and genres." ]
    ] 


comboIcon : T.Combo -> Html msg
comboIcon ((scope, (ensLabel, ensemble)) as model) =
  div [ class "box" ] 
    [ label [ class "label" ] [ text <| scope.label ++ " & " ++ ensLabel ]
    , Components.svg "ensemble"
    , p [ class "content" ] [ text <| (String.fromInt <| List.length ensemble) ++ " voices" ]
    ] 

  

  

synthDescription : T.Voice -> Html msg
synthDescription preset =
  div [class "column media-left"]
    [ div [class "media-content"]
    [ p [class "title is-4"] [text preset.label]
    ,roleDescription preset.role ] ]


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
  if density == 1 then 
    text "Very basic structure."
  else if density < 3 then 
    text "A good amount of structural variation for interest and stability."
  else
    text "A lot of motion, sometimes causing blurriness or obscurity."


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
  Components.card ("Voice: " ++ voice.label) <| div [ class "container" ] 
    [ Components.editText "Label" (text "") voice.label updateLabel
    , Components.editToggle "Duty" (T.Structure, "Structural") (T.Expression, "Expressive") voice.duty updateDuty
    , Components.editInt "Density" (densityMessage voice) D.rangeDensity voice.density updateDensity
    , Components.editInt "Complexity" (complexityMessage voice) D.rangeComplexity voice.complexity updateComplexity
    , Components.editSelection voice.role "Role" (roleDescription voice.role) options voice.role updateRole 
    ]


voiceEditor : List T.Voice -> (Maybe T.Voice) -> (Int -> msg) ->  ((Maybe T.Voice) -> msg) -> Html msg
voiceEditor voices voice select update =
  case voice of  
    Nothing ->
      voicePicker voices select 
 
    Just v ->
      editVoice v update


scopeEditor : List T.Scope -> (Maybe T.Scope) -> (Int -> msg) -> ((Maybe T.Scope) -> msg) -> Html msg
scopeEditor scopes scope select update =
  case scope of
    Nothing ->
      scopePicker scopes select
 
    Just s ->
      editScope s update


scopePicker : List T.Scope -> (Int -> msg) -> Html msg
scopePicker scopes select =
  Components.picker scopes scopeIcon select


voicePicker : List T.Voice -> (Int -> msg) -> Html msg
voicePicker voices select =
  Components.picker voices voiceIcon select


layoutPicker : List T.Layout -> (Int -> msg) -> Html msg
layoutPicker layouts select = 
  Components.picker layouts layoutIcon select


ensemblePicker : List T.NamedEnsemble -> (Int -> msg) -> Html msg
ensemblePicker ensembles select =
  Components.picker ensembles ensembleIcon select


instrumentEditor : T.Voice -> msg -> msg -> Html msg
instrumentEditor synth keep kill =
  div [class "column card is-half px-6 py-4", style "background" (D.roleColor synth.role)]
    []


csv : List String -> String
csv strs = 
  let 
    s = List.foldl (\str all -> all ++ str ++ "," )  "" strs 
  in
  String.slice 0 ((String.length s) - 1) s


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
  Components.card ("Scope: " ++ model.label) <| div [ class "scope-editor v3" ]
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


viewEnsembleWithRemover : T.NamedEnsemble -> (T.Voice -> msg) -> Html msg
viewEnsembleWithRemover (label, ensemble) remove =
  Components.box 
    [ p [ class "mb-3" ] [ text "Remove voices from this ensemble." ] 
    , div [ class "columns is-mobile is-multiline" ] <| 
       List.map (\voice -> 
        div [ class "column has-text-centered" ] 
          [ roleIcon voice.role
          , p [] [ text voice.label ]
          , span [class "delete", onClick (remove voice) ] [] ] ) ensemble ]


viewLayout : T.Layout -> Html msg
viewLayout (label, scopes) =
  Components.viewList scopes scopeIcon


viewLayoutWithRemover : T.Layout -> (T.Scope -> msg) -> Html msg
viewLayoutWithRemover ((label, scopes) as layout) remove =
  Components.box <|
    [ p [ class "mb-3"] [ text "Remove scopes from this layout." ]
    , div [ class "columns is-mobile is-multiline" ] <| 
        List.map (\scope -> 
          div [ class "column has-text-centered" ]
           [ scopeIcon scope
           , Components.deleteIcon (remove scope) ]) scopes]


ensembleAdder : List T.Voice -> (T.Voice -> msg) -> Html msg
ensembleAdder opts add =
  Components.box
    [ p [ class "mb-3" ] [ text "Add a voice to this ensemble" ]
    , div [ class "columns is-mobile is-multiline" ] <| 
      List.map (\voice -> 
        div [ class "column has-text-centered", onClick (add voice) ] 
          [ roleIcon voice.role
          , p [] [ text voice.label ] ] ) opts ]


layoutAdder : List T.Scope -> (T.Scope -> msg) -> Html msg
layoutAdder opts add =
  Components.box 
    [ p [ class "mb-3" ] [ text "Add a scope to this layout." ] 
    , div [ class "columns is-mobile is-multiline" ] <| 
        List.map (\scope -> 
          div [ class "column has-text-centered", onClick (add scope) ] [ scopeIcon scope ] ) opts ]


sectionAdder : List T.Scope -> List T.NamedEnsemble -> T.SectionP ->  Html msg
sectionAdder scopes ensembles (mScope, mEns)  =
  Components.box 
    [ p [ class "mb-3" ] [ text "Add a scope to this layout." ] 
    , div [ class "columns is-mobile is-multiline" ] <| 
        List.map (\scope -> 
          div [ class "column has-text-centered" ] [ scopeIcon scope ] ) scopes
    , div [ class "columns is-mobile is-multiline" ] <| 
        List.map (\ens -> 
          div [ class "column has-text-centered" ] [ ensembleIcon ens ] ) ensembles ]


ensembleNamer : T.NamedEnsemble -> (String -> msg) -> Html msg
ensembleNamer curr rename =
  Components.editText "Label" (text "") (Tuple.first curr) rename 


ensembleEditor : List T.Voice -> List T.NamedEnsemble -> (Maybe T.NamedEnsemble) -> (Int -> msg) -> ((Maybe T.NamedEnsemble) -> msg) -> (List T.NamedEnsemble -> msg) -> Html msg
ensembleEditor options ensembles current select updateCurrent updateAll =
  case current of 
    Nothing ->
      ensemblePicker ensembles select

    Just ((label, ens) as e) ->
      let 
        swapEns = (\ee -> Just ((Tuple.first e), ee))
      in 
      Components.card ("Ensemble: " ++ label) <| Components.wraps
        [ ensembleNamer e (\str -> updateCurrent (Just (str, (Tuple.second e)))) 
        , viewEnsembleWithRemover e (\voice -> updateCurrent (swapEns (Tools.remove (Just voice) (Tuple.second e))))
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


layoutIcon : T.Layout -> Html msg
layoutIcon (name, scopes) = 
  div [ class "box" ] 
    [ label [ class "label" ] [ text name ]
    , Components.svg "layout"
    , p [ class "content" ] [ text <| (String.fromInt <| List.length scopes) ++ " combos" ]
    ] 


layoutNamer : String -> (String -> msg) -> Html msg
layoutNamer title rename =
  let 
    content = "A nickname for this pattern of scopes.."
  in 
  Components.editText "Label" (text content) title rename 


layoutEditor : List T.Scope -> List T.Layout -> (Maybe T.Layout) -> (Int -> msg) -> ((Maybe T.Layout) -> msg) -> (List T.Layout -> msg) -> Html msg
layoutEditor options layouts current select updateCurrent updateAll =
  case current of 
    Nothing ->
      layoutPicker layouts select

    Just ((label, scopes) as l) ->
      Components.card ("Layout: " ++ label) <| Components.wraps <|
        [ layoutNamer label (\str -> updateCurrent (Just (str, scopes)))
        , viewLayout l 
        -- , viewLayoutWithRemover l (\layout -> updateCurrent (Just (label, (Tools.remove (Just layout) scopes))))
        , layoutAdder options (\scope -> updateCurrent (Just (label, Tools.conj scope scopes)))

        ]

    -- partial = List.filter (\(a, b) -> 
    --   case (a, b) of 
    --     (Just x, Just y) -> False
    --     _  -> True) combos


    -- complete = List.filter (\(a, b) ->
    --   case (a, b) of 
    --     (Just x, Just y) -> True
    --     _ -> False) combos


comboEditor : List T.Scope -> List T.NamedEnsemble ->  T.ComboP ->  ((Maybe T.ComboP) -> msg) -> Html msg
comboEditor scopes ensembles val toMsg =
  let
    updateFirst = (\second -> (\scope ->
      let
        next : T.ComboP
        next = (Just scope, Just second)
      in
      toMsg (Just next)))
    updateSecond = (\first -> (\ens ->
      let
        next : T.ComboP
        next = (Just first, Just ens)
      in
      toMsg (Just next)))
    justFirst = (\first -> toMsg (Just (Just first, Nothing)))
    justSecond = (\second -> toMsg (Just (Nothing, Just second)))
  in 
  case val of
    (Just a, Just b) -> 
      Components.colsMulti <|
      [ div [ class "column is-full" ] 
         [ div [ class "delete", onClick (justSecond b) ] [] 
         , scopeIcon a  ]
      , div [ class "column is-full" ] 
         [ div [ class "delete", onClick (justFirst a) ] [] 
         , ensembleIcon b ] 
      , Components.button (toMsg Nothing) [] "Done"
      ]

    (Nothing, Nothing) -> 
      let
        updateEnsemble : T.NamedEnsemble -> msg
        updateEnsemble = (\ens -> toMsg (Just (Nothing, Just ens)))
        updateScope : T.Scope -> msg
        updateScope = (\scope -> toMsg (Just (Just scope, Nothing)))
      in
      Components.wraps <|
        [ label [ class "has-background-danger subtitle"] [ ]
        , p [ ] [ text "Select a scope for this combo." ]
        , Components.cols <|
           List.map (\ens ->
             Components.col [ class "has-text-centered", onClick (updateScope ens)] [ scopeIcon ens ]) scopes
        , p [ ] [ text "Select an ensemble for this combo." ]
        , Components.cols <|
           List.map (\ens ->
             Components.col [ class "has-text-centered",  onClick (updateEnsemble ens)] [ ensembleIcon ens ]) ensembles  ]

    (Just a, Nothing) -> 
      let
        updateEnsemble = updateSecond a
      in
      Components.wraps <|
        [ p [ ] [ text "Select an ensemble for this combo." ]
        , Components.cols <|
           List.map (\ens ->
             Components.col [ onClick (updateEnsemble ens)] [ ensembleIcon ens ])  ensembles ]

    (Nothing, Just b) ->
      let
        updateScope = updateFirst b
      in
      Components.wraps <|
        [ label [ class "has-background-warning subtitle"] [ text "Needs a scope." ] 
        , p [ ] [ text "Select a scope for this combo." ]
        , Components.cols <|
           List.map (\scope ->
             Components.col [ onClick (updateScope scope)] [ scopeIcon scope ])  scopes ]




comboCompleteIcon : T.Combo -> Html msg
comboCompleteIcon ((scope, ensemble) as model) =
  div [ class "columns is-multiline" ] 
   [ div [ class "column is-full" ] [ scopeIcon scope ]
   , div [ class "column is-full" ] [ ensembleIcon ensemble ] ]


comboIncompleteIcon : T.ComboP -> Html msg
comboIncompleteIcon ((scope, ensemble) as model) =
  case model of
    (Just a, Just b) -> 
      comboCompleteIcon (a,b)

    (Nothing, Nothing) -> 
      label [ class "has-background-danger subtitle"] [ text "Needs a scope and an ensemble." ]

    (Just a, Nothing) -> 
      label [ class "has-background-warning subtitle"] [ text "Needs an ensemble." ]

    (Nothing, Just a) ->
      label [ class "has-background-warning "] [ text "Needs a scope." ]


viewComboP : T.ComboP -> Html msg
viewComboP  ((mScope, mEnsemble) as model)  =
  case model of
    (Just a, Just (eLabel, b)) -> 
      Components.box <|
          [ label [ class "label" ] [ text a.label ]
          , label [ class "label" ] [ text eLabel ]
          ]

    (Nothing, Nothing) -> 
      label [ class "subtitle"] [ text "Select a scope and an ensemble." ]

    (Just a, Nothing) -> 
      label [ class "subtitle"] [ text "Select an ensemble." ]

    (Nothing, Just a) ->
      label [ class "subtitle"] [ text "Select a scope." ]
       

viewTemplate : T.Template -> Html msg
viewTemplate ((mMeta, mCombos) as template) =
  Components.wraps <| List.map viewComboP mCombos


templateEditor : List T.Scope -> List T.NamedEnsemble -> T.Template ->  Maybe T.ComboP ->  (Maybe T.ComboP -> msg) -> Html msg
templateEditor scopes ensembles ((meta, combos) as template) curr updateCombo =
  case curr of
    Nothing -> 
      Components.picker combos viewComboP (\i -> updateCombo (Tools.get i combos))
 
    Just cp ->
      Components.card (Maybe.withDefault "This template has no title."  meta.title)  <| Components.wraps <|
        [ viewTemplate template
        , comboEditor scopes ensembles cp updateCombo ]


main =
  text ""

