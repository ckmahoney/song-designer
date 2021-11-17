module Update exposing (..)

import Html
import Random
import Debug
import Array 
import Time
import Browser

import Types as T
import Data as D


type UpdateMsg
  = Tick Time.Posix
  | UpdateSynthRole T.Voice T.SynthRole  
  | NewID Int
  | RequestPreset
  | NewPreset Int
  | AddPreset T.Voice
  | UpdatePreset (Maybe T.Voice)
  | KillPreset T.Voice
  | ChangeSelection (Maybe T.Voice)
  | UpdateDensity Int
  | UpdateComplexity Int
  | ChangePreset T.Ensemble


-- Units -- 

-- T.SynthPreset
-- T.Ensemble = List T.SynthPreset

-- T.Compo
-- T.Section = (T.Compo, T.Ensemble)

-- T.Score = List T.Section


type alias Meta = String


type alias Everything =
  { title : Meta
  , ensembles : List T.Ensemble
  , scopes : List T.Scope
  , score : List T.Section
  }


type Main
  = ManageEnsemble (List T.Ensemble)
  | ManageScopes (List T.Scope)
  | ManageScore (List T.Section)
  | EditMeta Meta
  | SeeDash Everything


-- All configurable values for a Voice
type EditVoice
  = UVoice T.Voice
  | UVoiceRole T.SynthRole
  | UVoiceDuty T.SynthDuty


updateVoiceEditor : EditVoice -> T.VoiceEditor -> (T.VoiceEditor, Cmd EditVoice)
updateVoiceEditor msg model =
  case msg of
    UVoice next ->
      ({ model | current = Just next }, Cmd.none)

    _ -> 
      (model, Cmd.none)

type EditEnsemble 
  = AddSynth T.Voice
  | KillSynth Int
  | SelectSynth Int 
  | UpdateSynth Int T.Voice


type EditLayout
  = RequestScope
  | NewScope Int
  | KillScope T.Scope
  | UpdateCPS Float
  | UpdateCPC Int
  | UpdateSize Int
  | UpdateRoot Int
  | UpdateLabel String
  | ChangeLayoutSelection (Maybe T.Scope)
  | AddLayoutSection T.Scope
  | RemoveLayoutAt Int


type EditScope
  = UScope T.Scope

type EditScore
  = RequestScore
  | NewScore Int
  | NewSection
  | ApplySection T.Section
  | UpdateSection T.Scope T.Ensemble
  | UpdateScope (Maybe T.Scope)
  | UpdateEnsemble (Maybe T.Ensemble)


ptoInt : Time.Posix -> Int
ptoInt t =
  Time.posixToMillis t


conj x xs =
  List.append xs [x]


rint : Random.Generator Int
rint =
  Random.int 1 1000


genID : Cmd UpdateMsg
genID = 
  Random.generate NewID rint


genPreset : Cmd UpdateMsg
genPreset = 
  Random.generate NewPreset rint


genScore : Cmd EditScore
genScore = 
  Random.generate NewScore rint


uuid : Random.Generator Int
uuid = 
  Random.int 1 100000000000000000000


createPreset : Int -> T.Voice
createPreset id_ =   
  let 
    ref = D.p1
  in
  { ref | id = id_ }

createScope : Int -> T.Scope
createScope id_ =   
  let 
    ref = D.s1
  in
  { ref | id = id_ }


createScore : Int -> T.Score
createScore id =
  []


remove : Maybe a -> List a -> List a
remove x xs =
  case x of 
   Nothing ->
     xs

   Just jx ->
     List.filter (\a -> not (jx == a)) xs


removeAt : Int -> List a -> List a
removeAt index els =
  case index of 
  0 ->
    List.drop 1 els

  n -> 
    List.concat [ (List.take n els), (List.drop (n + 1) els) ]


replace : a -> a -> List a -> List a 
replace prev next xs =
  let
    arr = Array.fromList xs
  in
  Array.set (D.findIndex prev xs) next arr
  |> Array.toList


noCmd : a -> (a, Cmd UpdateMsg)
noCmd x =
  (x, Cmd.none)


reindexScope : T.EditLayout -> T.EditLayout
reindexScope ({ index, current, presets } as model) =
  model


genScope : Cmd EditLayout
genScope =
  Random.generate NewScope rint


updateScore : EditScore -> T.EditScore -> (T.EditScore, Cmd EditScore)
updateScore msg model =
  case msg of 
    RequestScore ->
      (model, genScore)
 
    NewScore id ->
      let
          next = createScore id
      in
      ({ model | list = next }, Cmd.none)

    NewSection ->
      ({ model | current = Just (D.s1, D.kitBeat) }, Cmd.none)

    ApplySection (scope, ensemble) ->
      ({ model | current =  Just (scope, ensemble) }, Cmd.none)

    UpdateSection scope ensemble ->
      ({ model | current =  Just (scope, ensemble) }, Cmd.none)

    UpdateScope c ->
      ({ model | scope = c }, Cmd.none )

    UpdateEnsemble e ->
      ({ model | ensemble = e }, Cmd.none )


updateLayout : EditLayout -> T.EditLayout -> (T.EditLayout, Cmd EditLayout)
updateLayout msg model =
  case msg of
    RequestScope ->
      case model.current of 
        Nothing ->
          (model, genScope)

        Just prev ->
          (reindexScope model, genScope)

    NewScope id ->
      let 
        next = createScope id
      in       
      ({ model | current = Just next }, Cmd.none)

    KillScope scope ->
      let
        ps = remove (Just scope) model.presets
      in       
      ({ model
       | current = Nothing
       , presets = ps }, Cmd.none)

    UpdateCPS x ->
      case model.current of 
        Nothing -> 
          (model, Cmd.none)

        Just prev ->
          let 
            next = { prev | cps = x }
          in 
          ({ model | current = Just next }, Cmd.none)

    UpdateCPC x ->
      case model.current of 
        Nothing -> 
          (model, Cmd.none)

        Just prev ->
          let 
            next = { prev | cpc = x }
          in 
          ({ model | current = Just next }, Cmd.none)

    UpdateSize x ->
      case model.current of 
        Nothing -> 
          (model, Cmd.none)

        Just prev ->
          let 
            next = { prev | size = x }
          in 
          ({ model | current = Just next }, Cmd.none)

    UpdateRoot x ->
      case model.current of 
        Nothing ->
          (model, Cmd.none)

        Just prev ->
          let 
            next = { prev | root = x }
          in 
          ({ model | current = Just next }, Cmd.none)

    UpdateLabel string ->
      case model.current of 
        Nothing ->
          (model, Cmd.none)

        Just prev ->
          let 
            next = { prev | label = string }
          in 
          ({ model | current = Just next }, Cmd.none)
    
    ChangeLayoutSelection next ->
        case model.current of 
           Nothing ->
             case next of 
               Nothing ->
                 (model, Cmd.none)

               Just comp ->
                 ({ model 
                 | current = next
                 , presets = remove (Just comp) model.presets }, Cmd.none)

           Just prev ->
             let 
               ps = List.append model.presets [prev]
             in 
             case next of 
               Nothing -> 
                 ({ model 
                 | current = next
                 , presets = ps }, Cmd.none)

               Just comp -> 
                 ({ model 
                 | current = next
                 , presets = remove (Just comp) ps }, Cmd.none)
    
    AddLayoutSection scope -> 
      ({ model | list = conj scope model.list }, Cmd.none)

    RemoveLayoutAt index -> 
      ({ model | list = removeAt index model.list }, Cmd.none)


updateEnsembleEditor : EditEnsemble ->  T.EnsembleEditor -> (T.EnsembleEditor, Cmd EditEnsemble)
updateEnsembleEditor msg model =
  case msg of 
    AddSynth inst ->
      (model, Cmd.none)

    KillSynth index ->
      (model, Cmd.none)      

    SelectSynth index ->
      (model, Cmd.none)      

    UpdateSynth index inst ->
      (model, Cmd.none)      


updateEnsemble : UpdateMsg -> T.VoiceEditor -> (T.VoiceEditor, Cmd UpdateMsg)
updateEnsemble msg model =
    case msg of
      Tick ptime ->
        noCmd { model | time = ptoInt ptime }

      RequestPreset ->
        (model, genPreset)

      NewID int ->
        case model.current of
          Nothing ->
            noCmd { model | current = Just (createPreset int) }

          Just prev ->   
            let
              next = { prev | id = int } 
              presets = replace prev next model.presets
             in
             noCmd { model | current = Just next } 
 
      NewPreset id ->
       let
         next = createPreset id
       in      
         noCmd { model | current  = Just next }
               -- , presets = conj next model.presets }


      AddPreset preset ->
        noCmd { model 
              | presets = conj preset model.presets }


      UpdatePreset next ->
        noCmd { model | current = next }


      KillPreset preset ->
       let 
         yy = Debug.log "presets:" model.presets
       in
       noCmd { model 
             | current = Nothing
             , presets = remove (Just preset) model.presets }

      -- remove the target from presets and hold it in current   
      ChangeSelection next ->
        case model.current of 
         Nothing ->
           case next of 
             Nothing ->
               noCmd model

             Just n ->
               noCmd { model 
                     | current = next
                     , presets = remove (Just n) model.presets }

         Just prev ->
           let 
             ps = List.append model.presets [prev]
           in 
           case next of 
             Nothing -> 
               noCmd { model 
               | current = next
               , presets = ps }

             Just n -> 
               noCmd { model 
               | current = next
               , presets = remove (Just n) ps }

      UpdateSynthRole curr r ->
        let 
          next = { curr | role = r
                 , label = (Tuple.second <| D.roleLabel r) }
          -- index = D.findIndex curr model.presets
          -- presets = Array.toList <| Array.set index next <| Array.fromList model.presets
        in 
         noCmd { model | current = Just next }


      UpdateDensity val ->
        case model.current of 
         Nothing ->
          noCmd model
         
         Just prev ->
           let
             next = { prev | density = val }
           in
           noCmd { model | current = Just next }


      UpdateComplexity val ->
       case model.current of 
        Nothing ->
          noCmd model

        Just prev ->
           let
             next = { prev | complexity = val }
           in
           noCmd { model | current = Just next }

      ChangePreset kit ->
       noCmd { model | presets = kit }

main =
  Html.text ""
