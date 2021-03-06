module Components.View exposing (..)
-- Html components specific to Synthony 

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)

import Components.Components as Components

import Array
import Defs.Types as T
import Defs.Data as D
import String exposing (String(..))
import Tools


useSharps = False


border pos =
  [ Attr.style ("border-" ++ pos) "1px solid rgba(0,0,0,0.1)"
  , Attr.style "border-radius" "5px"
  ]



scopePeek : T.Scope -> Html msg
scopePeek state =
  Components.colsWith [Attr.class "is-vcentered"] <|
    [ Components.colHalf <| h1 [Attr.class "is-size-4"] [text  state.label ]
    , Components.colHalf <| div [Attr.class "columns has-text-right is-size-5"]
       [ Components.colHalf <| text <| Components.keyMessage True state.root
       , Components.colHalf <| text <| scopeTimeString state
       ]
    ] 



ensemblePeek : T.Ensemble -> Html msg
ensemblePeek state =
   Components.colsWith [Attr.class "is-multiline"]
     <| if List.length state == 0 then 
       [ text "No voices in this ensemble." ] else 
       List.map (\{role} -> Components.col [] <| [ Components.svg  (Tuple.first <| D.roleLabel role)]) state


ensembleThumb : T.Ensemble -> Html msg
ensembleThumb state =
   div []   <| List.singleton <| Components.colsWith [Attr.class "is-multiline is-mobile"]
     <| if List.length state == 0 then 
       [ Components.paragraph "They say silence is golden. You'll get a lot of gold with no voices."
       , Components.paragraph " Click the Settings icon to add your first voice to this ensemble." ] else 
       List.map (\{role} ->       
         div [ class "column is-one-third is-flex is-justify-content-center"
             , style "background" (D.roleColor role)]
               [ img [ class "is-block" 
               , width 50
               , height 50
               , src <| "/assets/svg/" ++ (Tuple.first <| D.roleLabel role) ++ ".svg"] [] ]) state

keyNames =
  if useSharps then D.sharps else D.flats


keyLabel : Int -> String
keyLabel key =
  Maybe.withDefault "No key found for that." <| Tools.get key keyNames


roleRow : Html msg
roleRow = 
  div [] (List.map roleIcon D.roles)


roleDescription : T.SynthRole -> Html msg
roleDescription role =
  div [ class "content" ] [ b [] [ text <| Tuple.first <| D.roleLabel role],  p [] [text <| D.roleDescription role ] ]


labelInfo : Html msg
labelInfo =
  div [class "content"] [ text "Descriptive name for this scope of music." ]


templateMessage : Html msg
templateMessage =
  Components.box 
    [ label [ class "subtitle  mb-3" ] [ text "Use a template to create a new score from a layout you like." ]
    , p [] [ text "You can borrow common song structures, so your music has a structure that is easy to understand."  ] 
    , p [] [ text "Repeated structure also defines a prodcuer's style. When you make many songs with the same template, it becomes one of your signatures." ]
    ] 


getTimes : List T.Scope -> List (Float, Int)  -- (cps, dur)
getTimes scopes  =
  List.map (\{cps,cpc,size} -> (cps, sizeToCycles cpc size)) scopes


-- returns duration in seconds
layoutDuration : List T.Scope -> Float
layoutDuration scopes =
  List.foldl (\(cps, nCycles) total -> total + (cps * toFloat nCycles)) 0 (getTimes scopes)


templateDuration : T.Template  -> Float
templateDuration (meta, layout ) =
 let
   scopes = List.map Tuple.first layout
 in 
  List.foldl (\(cps, nCycles) total -> total + (meta.cps * (toFloat nCycles))) 0 (getTimes scopes)


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
  img [ class "is-block mx-auto"
      , width 50
      , height 50
      , src <| "/assets/svg/" ++ (Tuple.first <| D.roleLabel role) ++ ".svg"] []


roleIconColored : T.SynthRole -> Html msg
roleIconColored role =
  div [ class "box"
      , style "background" (D.roleColor role)]
  [ img [ class "mx-auto is-block" 
          , width 50
          , height 50
          , src <| "/assets/svg/" ++ (Tuple.first <| D.roleLabel role) ++ ".svg"] [] ]


ensembleIcon : T.Ensemble -> Html msg
ensembleIcon  ensemble = 
  div [ class "box" ] 
    [ label [ class "label" ] [ text "" ]
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
templateIcon (meta, mCombos) =
  div [ class "container" ] 
    [ label [ class "label" ] [ text meta.title ]
    , Components.svg "score"
    , p [ class "content" ] [ text "This is a template for making scores. Use it as a basis for making variations of styles and genres." ]
    ] 


comboIcon : T.Combo -> Html msg
comboIcon ((scope, ensemble) as model) =
  div [ class "box" ] 
    [ label [ class "label" ] [ text <| scope.label ]
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
  case complexity of
    1 ->
      text "Very basic harmonics."
    2 ->
      text "Compelling harmonic motion without getting too far out."
    3 ->
      text "Higher depths of harmony with ambiguous results."
    _ ->
      text "Too complex for this bod"


densityMessage : T.Voice -> Html msg
densityMessage {density} =
  case density of 
    1 ->
      text "Clear outlines with no embellishments. A basic structure."
    2->
      text "Embelleshments and variations for featured parts."
    3 ->
      text "A lot of motion, induces more energy but requires more focus."
    _ ->
      text "Too dense for this bod"


viewVoice : T.Voice -> Html msg
viewVoice voice =
  Components.box 
    [ label [ class "label" ] [ text voice.label ] 
    , Components.colsWith [] 
        [ div [ class "column is-one-third"] [ roleIcon voice.role ]
        , div [ class "column is-two-thirds"] [ roleDescription voice.role ]
        ]
    , p [] [ densityMessage voice ] 
    , p [] [ complexityMessage voice ] 
    ] 




editVoice : T.Voice -> ((Maybe T.Voice) -> msg) -> Html msg -> Html msg ->  Html msg
editVoice voice sig buttSave buttDelete =
  let 
    justSig = (\x -> sig (Just x))
    updateComplexity = (\n -> justSig { voice | complexity = n })
    updateDensity = (\n -> justSig { voice | density = n })
    updateLabel = (\s -> justSig { voice | label = s })
    updateRole = (\r -> justSig { voice | role = r } )
    updateDuty = (\d -> justSig { voice | duty = d } )
    options = List.map (\r -> (r, roleIcon r)) D.roles
  in
  Components.card2 ("Editing Voice" ++ voice.label) [buttSave, buttDelete] <|
   div [ class "container" ] 
    [ Components.colsWith [ class "is-mobile is-flex is-justify-content-space-between" ] 
        [ Components.colHalf <| viewVoice voice
        , Components.colHalf <| Components.editText "Label" (text "") voice.label updateLabel
        ]  
    , Components.editSelection voice.role "Role" (text "") options voice.role updateRole 
    , Components.editToggle "Duty" (T.Structure, "Structural") (T.Expression, "Expressive") voice.duty updateDuty
    , Components.colsWith [ class "is-mobile is-flex is-justify-content-space-between" ] 
       [ Components.colHalf <| Components.editInt "Density" (densityMessage voice) D.rangeDensity voice.density updateDensity
       , Components.colHalf <| Components.editInt "Complexity" (complexityMessage voice) D.rangeComplexity voice.complexity updateComplexity
       ]
    ]


editMeta : T.ScoreMeta -> (T.ScoreMeta -> msg) -> Html msg
editMeta meta toMsg =
  let 
    updateCPC = (\int -> toMsg { meta | cpc = int })
    updateCPS = (\int -> toMsg { meta | cps =  ((toFloat int) / 60) })
    updateRoot = (\int -> toMsg { meta | root = Maybe.withDefault 15.0 (Tools.get int D.chromaticRoots) })
  in 
  Components.box
    [ Components.editText "Tag" (text "") meta.title (\t -> toMsg { meta | title = t })
    , Components.editInt "Meter" (meterMessage meta.cpc) D.rangeCPC meta.cpc updateCPC
    , Components.editInt "Tempo" (metaTempoMessage meta.cps) (50, 260) (round (meta.cps * 60)) updateCPS 
    , Components.keyPicker useSharps (Tools.findIndex meta.root D.chromaticRoots) updateRoot
    ]


editTemplateCombos : List T.Voice -> List T.Scope -> T.Layout ->  (( T.Layout) -> msg) -> Html msg -> Html  msg -> Html msg
editTemplateCombos voices scopes ( combos as layout) toMsg buttSave buttDelete =
  Components.box <|
    List.map comboIcon combos
    -- [
  -- ;; i already hav ea list of (Scope, Ensemble) to visualize the score. Let me do that first.
    -- ]


editTemplate : T.Template -> List T.Scope -> List T.Voice -> (T.Template -> msg) -> Html msg -> Html msg ->  Html msg
editTemplate ((scoreMeta, layout) as template) scopes voices sig buttSave buttDelete =
  let
    uMeta = (\meta -> 
      let
       next = (meta, layout)
      in 
        sig (next))
    uLayout = (\newCombos -> 
      let
       next = (scoreMeta, newCombos)
      in 
        sig (next))
  in 
  Components.card2 ("Editing Template" ++ scoreMeta.title) [buttSave, buttDelete] <|
   div [ class "container" ]
     [ editMeta scoreMeta uMeta 
     , editTemplateCombos voices scopes layout uLayout (text "save") (text "delete") 
     ]


addAnother : msg -> Html msg
addAnother msg = 
  div [ class "is-size-6 has-background-black has-text-white  box column is-vcentered has-text-centered", onClick msg ] [ text "Add Another" , span [] [text "+"] ] 


rolePicker : List T.SynthRole -> (Int -> msg) -> Html msg
rolePicker roles select =
  Components.picker roles roleIcon select


scopePicker : List T.Scope -> (Int -> msg) -> msg -> Html msg
scopePicker scopes select createNew  =
  Components.pickerAnd scopes (addAnother createNew) scopeIcon select


voicePicker : List T.Voice -> (Int -> msg) -> msg -> Html msg
voicePicker voices select createNew =
  Components.pickerAnd voices (addAnother createNew) voiceIcon select


scorePicker : List T.Score -> (Int -> msg) -> msg -> Html msg
scorePicker scores select createNew =
  Components.pickerAnd scores (addAnother createNew) scoreIcon select


comboPicker : List T.Combo -> (Int -> msg) -> msg -> Html msg
comboPicker combos select createNew = 
  Components.pickerAnd combos (addAnother createNew) comboIcon select


comboPickerKiller : List T.Combo -> (Int -> msg) -> msg -> (Int -> msg) -> Html msg
comboPickerKiller combos select createNew kill = 
  Components.pickerKillerAnother combos (addAnother createNew) comboIcon select kill


layoutPickerOld : List T.Layout -> (Int -> msg) -> Html msg
layoutPickerOld layouts select = 
  Components.picker layouts layoutIcon select


layoutPicker : List T.Layout -> (Int -> msg) -> msg -> Html msg
layoutPicker layouts select createNew = 
  Components.pickerAnd layouts (addAnother createNew) layoutIcon select


templatePicker : List T.Template -> (Int -> msg) -> msg -> Html msg
templatePicker templates select createNew = 
  Components.pickerAnd templates (addAnother createNew) templateIcon select



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


tempoMessage : Int -> Float -> Int -> Html msg
tempoMessage cpc cps size =
  div [class "content"] 
    [ p [] [text "The speed for this element. Higher BPM are faster, and lower BPM are slower."]
    , p [] [ text <| "With size " ++ (String.fromInt size) ++ " at " ++ (bpmString cps) ++ "BPM, that means this section is " ++ (String.fromInt (round <| duration cpc cps size)) ++  " seconds long." ] ]


metaTempoMessage :  Float -> Html msg
metaTempoMessage  cps  =
  div [class "content"] 
    [ p [] [text "The global playback rate for this song."
      , label [ class "label" ] [ text <| (String.fromFloat (60 * cps) ++ " BPM") ] ]
    , p [] [text "The tempo of each scope is relative to this global playback rate."]
    ]



meterMessage : Int -> Html msg
meterMessage cpc =
  p [] [ text <| "This section is in " ++ (timeSigString cpc) ++ " time." ]


sizeToCycles: Int -> Int -> Int
sizeToCycles cpc size =
 cpc * (2^(size-1))


duration : Int -> Float -> Int -> Float
duration cpc cps size  =
  (1/cps) * (toFloat (sizeToCycles cpc size))

durString : Int -> Float -> Int -> String
durString cpc cps size  =
  String.fromInt <| round <| duration cpc cps size  


-- Given a Float seconds, returns a MM:SS string
timeString : Float -> String
timeString t =
  let
    m = ((round t) // 60)
    pad = if m < 10 then "" else "" 
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


sizeMessage : Int -> Int -> Html msg
sizeMessage cpc size =
  div [] 
    [ p [] [ text <| 
   if size < 3 then
      "This is a short part, "
   else if size < 7 then
     "This part is medium length, "
   else
      "This part is large, "
   , p [] [ text <| "and the phrases will feel like they are about " ++ String.fromInt cpc ++ " beats long." ] ] ]


totalLength : List T.Scope -> Float
totalLength score =
  List.foldl (\{cpc, cps, size} sum -> 
    sum + (duration cpc cps size)) 0 score


countStems : List T.Section -> Int
countStems score =
  List.foldl (\(compo, ensemble) sum ->  
    sum + (List.length ensemble)) 0 score


editScope : T.Scope -> ((Maybe T.Scope) -> msg) -> Html msg -> Html msg -> Html msg
editScope model sig buttSave buttDelete = 
  let
    justSig = (\x -> sig (Just x))
    updateLabel = (\str -> justSig { model | label = str })
    updateCPC = (\int -> justSig { model | cpc = int })
    updateSize = (\int -> justSig { model | size = int })
    updateCPS = (\flt -> justSig { model | cps = flt })
    updateRoot = (\flt -> justSig { model | root = flt })
  in 
    Components.card2  ("Scope: " ++ model.label) [buttSave, buttDelete] <| div [ class "scope-editor v3" ]
     [ Components.editText "Label" labelInfo model.label updateLabel
     , Components.editInt "Meter" (meterMessage model.cpc) D.rangeCPC model.cpc updateCPC
     , Components.editInt "Size" (sizeMessage model.cpc  model.size) D.rangeScopeSize model.size updateSize 
     , Components.editRange "Tempo" (tempoMessage model.cpc model.cps model.size) D.rangeCPS model.cps updateCPS 
     , Components.keyPicker False model.root updateRoot 
     ] 


editScopeOld : T.Scope -> ((Maybe T.Scope) -> msg) -> Html msg 
editScopeOld model sig =
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
   , Components.editInt "Meter" (meterMessage model.cpc) D.rangeCPC model.cpc updateCPC
   , Components.editInt "Size" (sizeMessage model.cpc model.size) D.rangeScopeSize model.size updateSize 
   , Components.editRange "Tempo" (tempoMessage  model.cpc model.cps model.size) D.rangeCPS model.cps updateCPS 
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




viewLayoutWithRemover : T.Layout -> (T.Combo -> msg) -> Html msg
viewLayoutWithRemover combos remove =
  Components.box <|
    [ p [ class "mb-3"] [ text "Remove combos from this layout." ]
    , div [ class "columns is-mobile is-multiline" ] <| 
        List.map (\combo -> 
          div [ class "column has-text-centered" ]
           [ comboIcon combo
           , Components.deleteIcon (remove combo) ]) combos]


ensembleAdder : List T.Voice -> (T.Voice -> msg) -> Html msg
ensembleAdder opts add =
  Components.box
    [ p [ class "mb-3" ] [ text "Add a voice to this ensemble" ]
    , div [ class "columns is-mobile is-multiline" ] <| 
      List.map (\voice -> 
        div [ class "column has-text-centered", onClick (add voice) ] 
          [ roleIcon voice.role
          , p [] [ text voice.label ] ] ) opts ]



sectionAdder : List T.Scope -> List T.Ensemble -> T.SectionP ->  Html msg
sectionAdder scopes ensembles (mScope, mEns)  =
  Components.box 
    [ p [ class "mb-3" ] [ text "Add a scope to this layout." ] 
    , div [ class "columns is-mobile is-multiline" ] <| 
        List.map (\scope -> 
          div [ class "column has-text-centered" ] [ scopeIcon scope ] ) scopes
    , div [ class "columns is-mobile is-multiline" ] <| 
        List.map (\ens -> 
          div [ class "column has-text-centered" ] [ ensembleIcon ens ] ) ensembles ]



scopeIcon : T.Scope -> Html msg
scopeIcon scope =
  div [ class "box has-text-centered" ] 
    [ label [ class "label" ] [ text scope.label ]
    , Components.svg "scope"
    , div [ class "content" ]
      [ p [] [ text "Size ", b [] [text <| String.fromInt scope.size ] ]
    , p [] [ text (scopeTimeString scope) ] 
    ] ]


voiceIcon : T.Voice -> Html msg
voiceIcon voice = 
  div [ class "box mx-3 my-0 has-text-centered" ] 
    [ label [ class "label" ] [ text voice.label ]
    , roleIcon voice.role
    , p [ class "content" ] [ text (Tuple.first <| D.roleLabel voice.role) ]
    ]

synthIconHelpMobile : T.SynthRole ->  (T.SynthRole -> msg)  -> Bool -> (T.SynthRole -> msg) -> msg -> Html msg
synthIconHelpMobile role click showHelp toggleHelp playSample =
  div [] 
    [ div [class "voice-help content", class <| if showHelp then "visible" else "hidden"] 
        [text <| D.synthHelp role] 
    , div [class "is-block"] 
       [ div [class "box mb-0 mx-3", onClick (click role) ] [ roleIcon role ]
       , p [ class "mt-2 has-text-centered"] [ text <| D.roleName role ]
       ]
       , synthDrawerMobile role showHelp (toggleHelp role) playSample
    ]
  
synthIconHelpDesktop : T.SynthRole ->  (T.SynthRole -> msg)  -> Bool -> (T.SynthRole -> msg) -> msg -> Html msg
synthIconHelpDesktop role click showHelp toggleHelp playSample =
  div [] 
    [ div [class "voice-help content", class <| if showHelp then "visible" else "hidden"] 
        [ text <| D.synthHelp role ] 
    , div [class "is-flex"] 
       [ div [class "box mb-0", onClick (click role) ] [ roleIcon role ]
       , synthDrawerDesktop role showHelp (toggleHelp role) playSample
       ]
    , p [ class "mt-2 has-text-centered"] [ text <| D.roleName role ]
    ]
  

synthIconHelp : T.SynthRole -> (T.SynthRole -> msg) -> Bool -> (T.SynthRole -> msg) -> msg -> Html msg
synthIconHelp role click showHelp toggleHelp playSample =
  div [] 
    [ Components.mobileOnly <| synthIconHelpMobile role click showHelp toggleHelp playSample 
    , Components.desktopOnly <| synthIconHelpDesktop role click showHelp toggleHelp playSample 
    ]

synthDrawerMobile : T.SynthRole -> Bool -> msg -> msg -> Html msg
synthDrawerMobile role showHelp toggleHelp toggleSample =
 let
  children = 
    [ div [onClick toggleHelp] [ Components.iconWith "bg-info" "question-mark"]
    , div [onClick toggleSample] [ Components.icon "speaker" ]
    ]
 in 
  div [Components.flexRow, class "px-5 mb-0 is-justify-content-space-around is-clickable" ] children


synthDrawerDesktop : T.SynthRole -> Bool -> msg -> msg -> Html msg
synthDrawerDesktop role showHelp toggleHelp toggleSample =
 let
  children = 
    [ div [onClick toggleHelp] [ Components.iconWith "bg-info" "question-mark"]
    , div [onClick toggleSample] [ Components.icon "speaker" ]
    ]
 in 
 div [ Components.mobileNotClass, Components.flexColumn, class "px-5 mb-0 is-justify-content-space-around is-clickable" ] children



synthDrawer : T.SynthRole -> Bool -> msg -> msg -> Html msg
synthDrawer role showHelp toggleHelp toggleSample =
 let
  children = 
    [ div [onClick toggleHelp] [ Components.icon "question-mark"]
    , div [onClick toggleSample] [ Components.icon "speaker" ]
    ]
 in 
  div []
    [ div [ Components.mobileOnlyClass, Components.flexRow, class "px-5 mb-0 is-justify-content-space-around is-clickable" ] children
    , div [ Components.mobileNotClass, Components.flexColumn, class "px-5 mb-0 is-justify-content-space-around is-clickable" ] children
   ]


layoutIcon : T.Layout -> Html msg
layoutIcon  scopes = 
  div [ class "box" ] 
    [ label [ class "label" ] [ text "Ensemble" ]
    , Components.svg "layout"
    , p [ class "content" ] [ text <| (String.fromInt <| List.length scopes) ++ " combos" ]
    ] 


layoutNamer : String -> (String -> msg) -> Html msg
layoutNamer title rename =
  let 
    content = "A nickname for this structure. Use generic terms such as \"Trap Ballad\" or \"Upbeat and Happy\""   
  in 
  Components.editText "Label" (text content) title rename 



comboPreview : T.Combo -> Html msg
comboPreview ((scope, ensemble) as combo) =
    Components.boxWith "my-3 p-3 has-background-dark columns" 
      <| [ div [ class "column is-one-quarter" ] [ scopeIcon scope ]
         , div [ class "column is-three-quarters" ] [ Components.colsMulti  
            (List.map (\voice -> div [class "column is-one-quarter" ][ (voiceIcon voice)]) ensemble) ] ]


viewLayout : T.Layout -> Html msg
viewLayout  combos  =
    Components.box  <| 
      [ label [ class "label mb-3" ] [text "Ensemble"]  ]
      ++ (List.map comboPreview combos)


editLayout : T.Layout -> (T.ComboP -> msg) -> (T.Combo -> msg ) -> (Int -> msg ) -> Html msg
editLayout combos edit create delete =
  if 0 == List.length combos then 
    Components.wrap <| Components.button (edit (Nothing, Nothing)) [] "Make the first Combo"
  else 
    let
      select = (\int -> 
        case Tools.get int combos of
           Nothing -> edit D.emptyComboP -- not gonna happen
           Just combo -> create combo)
    in
    Components.wrap <| comboPickerKiller combos select (edit (Nothing, Nothing)) delete


designLayout : T.Layout -> List T.Scope -> List T.Voice -> ( T.Layout -> msg) -> (T.ComboP -> msg) -> (List T.Combo -> msg) -> Html msg -> Html msg -> Html msg
designLayout combos scopeOpts voiceOpts sig sigEditComboP updateCombos buttSave buttDelete  =
  let
    create = (\combo -> updateCombos (Tools.conj combo combos))
    delete = (\i -> updateCombos (Tools.removeAt i combos))
  in 
  Components.card2 "Editing ensemble" [buttSave, buttDelete] <|
   div [ class "container" ] <|
    [ editLayout combos sigEditComboP create delete
    ] 

   
editScore : T.Score -> (T.Score -> msg) -> Html msg -> Html msg -> Html msg
editScore score sig buttSave buttDelete = 
  text "editing score"


comboEditor : List T.Scope -> List T.Voice ->  T.ComboP ->  (T.ComboP -> msg) -> (T.Combo -> msg) -> Html msg
comboEditor scopes voices comboP contMsg doneMsg =
  let
    updateFirst = (\scope ->
      let
        next : T.ComboP
        next = (Just scope, Tuple.second comboP)
      in
      contMsg next)

    updateSecond = (\ens ->
      let
        next : T.ComboP
        next = (Tuple.first comboP, Just ens)
      in
      contMsg next)

    justFirst = (\first -> contMsg (Just first, Nothing))
    justSecond = (\second -> contMsg  (Nothing, Just second))

    updateEnsemble : T.Voice -> msg
    updateEnsemble = (\voice -> 
      (updateSecond  [voice]))

    updateScope : T.Scope -> msg
    updateScope = (\scope -> 
      (updateFirst  scope))

    pickingScope = Components.cols <|
      List.map (\scope ->
         Components.col [ class "has-text-centered", onClick (updateFirst scope)] [ scopeIcon scope ]) scopes

    pickingVoices = (\ens -> Components.cols <|
       List.indexedMap (\i voice ->
         Components.col [ class "has-text-centered",  onClick (updateSecond (Tools.conj voice ens))] [ voiceIcon voice ]) voices )

    showingScope = (\s -> 
      div [ class "column is-full" ] 
         [ div [ class "delete", onClick (justFirst s) ] [] 
         , scopeIcon s  ])
    showingEnsemble = (\e ->
      div [ class "column is-full" ] 
         [ div [ class "delete", onClick (justSecond e) ] [] 
         , ensembleIcon e ])
  in
  case comboP of
    (Nothing, Nothing) -> 
      Components.wraps <|
        [ label [ class "has-background-danger subtitle"] [ ]
        , p [ ] [ text "Select a scope for this combo." ]
        , pickingScope
        , p [ ] [ text "Add a voice to this combo." ]
        , pickingVoices [] ]
    (Nothing, Just mEns) -> 
      Components.colsMulti
        [ pickingScope
        , pickingVoices mEns
        ]
    (Just mScope, Nothing) -> 
      Components.colsMulti
        [ showingScope mScope
        , pickingVoices []
        ]
    (Just a, Just b) -> 
      Components.colsMulti
        [ showingScope a
        , showingEnsemble b 
        , Components.button (doneMsg (a,b)) [] "Done"
        ]


comboCompleteIcon : T.Combo -> Html msg
comboCompleteIcon ((scope, ensemble) as model) =
  div [ class "columns is-multiline" ] 
   [ div [ class "column is-full" ] [ scopeIcon scope ]
   , div [ class "column is-full" ] [ ensembleIcon ensemble ] ]


viewCombo : T.Combo -> Html msg
viewCombo  (({cpc,cps,size} as scope), ensemble)  =
 let
  nVoices = List.length ensemble
 in 
  Components.colsWith [Attr.class "thumb-combo-horizontal"] 
    [ Components.col ((Attr.class "is-half" :: (border "right"))++  (border "left")) [ scopePeek scope ]
    , Components.col ((Attr.class "is-half" :: (border "left"))  ++ (border "right")) [ ensembleThumb ensemble ]
    ]
       
viewComboVertical : T.Combo -> Html msg
viewComboVertical  (({cpc,cps,size} as scope), ensemble)  =
 let
  nVoices = List.length ensemble
 in 
  Components.colsWith [Attr.class "thumb-combo-vertical is-flex-direction-column columns"]
    [ Components.col ( (border "right")++  (border "left")) [ scopePeek scope ]
    , Components.col ( (border "left")  ++ (border "right")) [ ensembleThumb ensemble ]
    ]
       

viewTemplate : T.Template -> Html msg
viewTemplate ((scoreMeta, scopes) as template) =
  Components.box <|
    [ label [ class "title" ] [ text scoreMeta.title  ] ] ++ (List.map comboIcon scopes)


main =
  text ""

