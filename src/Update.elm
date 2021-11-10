module Update exposing (..)

import Types as T
import Html

type UpdateMsg
  = UpdateSynthRole T.SynthPreset T.SynthRole  
  | AddSynth T.SynthPreset
  | RemoveSynth T.SynthPreset 
  | Done


remove : a -> List a -> List a
remove x xs =
  List.filter (\a -> not (x == a)) xs


update : UpdateMsg -> T.State T.SynthPreset -> ( T.State T.SynthPreset, Cmd UpdateMsg )
update msg model =
    case msg of
      Done ->
        (model, Cmd.none)

      AddSynth preset ->
        ({model | presets = preset :: model.presets}, Cmd.none)

      UpdateSynthRole preset r ->
        let 
          updated = { model | presets = List.map (\x -> if x == preset then { preset | role = r } else x) model.presets }
        in 
        (updated, Cmd.none)

      RemoveSynth preset ->
        ({model | presets = remove preset model.presets }, Cmd.none)

main =
  Html.text ""
