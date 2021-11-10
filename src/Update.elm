module Update exposing (..)

import Types as T
import Html


type UpdateMsg
  = UpdateSynthRole T.SynthPreset T.SynthRole  
  | AddSynth T.SynthPreset
  | RemoveSynth T.SynthPreset 
  | UpdateDensity Int
  | UpdateComplexity Int


remove : a -> List a -> List a
remove x xs =
  List.filter (\a -> not (x == a)) xs


update : UpdateMsg -> T.State T.SynthPreset -> T.State T.SynthPreset
update msg model =
    case msg of
      AddSynth preset ->
        {model | presets = preset :: model.presets}

      UpdateSynthRole preset r ->
        let 
          updated = { model | presets = List.map (\x -> if x == preset then { preset | role = r } else x) model.presets }
        in 
        updated

      RemoveSynth preset ->
        {model | presets = remove preset model.presets }

      UpdateDensity val ->
        let 
          prev = model.current
          next = { prev | density = val }
        in
        { model | current = next }

      UpdateComplexity val ->
        let 
          prev = model.current
          next = { prev | complexity = val }
        in
        { model | current = next }

main =
  Html.text ""
