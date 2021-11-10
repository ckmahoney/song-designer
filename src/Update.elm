module Update exposing (..)

import Html
import Random
import Debug exposing (log)

import Types as T
import Data as D


type UpdateMsg
  = UpdateSynthRole T.SynthPreset T.SynthRole  
  | NewID Int
  | SavePreset T.SynthPreset
  | KillPreset T.SynthPreset
  | ChangeSelection T.SynthPreset
  | RemoveSynth T.SynthPreset 
  | UpdateDensity Int
  | UpdateComplexity Int



rint : Random.Generator Int
rint =
  Random.int 1 1000


genID : Cmd UpdateMsg
genID = 
  Random.generate NewID rint


uuid : Random.Generator Int
uuid = 
  Random.int 1 100000000000000000000
  

remove : a -> List a -> List a
remove x xs =
  List.filter (\a -> not (x == a)) xs


replace : a -> a -> List a -> List a 
replace x xx xs =
  let
    prev = x
    nexts = remove prev xs
  in
  xx :: nexts


noCmd : a -> (a, Cmd UpdateMsg)
noCmd x =
  (x, Cmd.none)


update : UpdateMsg -> T.State T.SynthPreset -> (T.State T.SynthPreset, Cmd UpdateMsg)
update msg model =
    let 
      xx = log "Looking at model.current:" model.current
      yy = log "Looking at modelpresets:" model.presets
    in

    case msg of
      NewID int ->
        let
          prev = model.current
          presets = remove prev model.presets
          next = { prev | id = int } 
        in
        noCmd { model | current = next} 

      SavePreset preset ->
       ({ model | presets = preset ::  model.presets }, genID)

      KillPreset preset ->
       ({ model | presets = remove preset model.presets }, Cmd.none)
  
      ChangeSelection preset ->
        noCmd <| { model | current = preset }

      UpdateSynthRole preset r ->
        let 
          prev = model.current
          next = { prev | role = r, title = (Tuple.second <| D.roleLabel r)}
          presets = List.map (\x -> if x == preset then next else x) model.presets
        in 
        noCmd <|
          { current = next
          , presets = presets }

      RemoveSynth preset ->
        noCmd <| {model | presets = remove preset model.presets }

      UpdateDensity val ->
        let 
          prev = model.current
          next = { prev | density = val }
        in
        noCmd <| { model | current = next, presets = replace prev next model.presets }

      UpdateComplexity val ->
        let 
          prev = model.current
          next = { prev | complexity = val }
        in
          noCmd <| { model | current = next, presets = replace prev next model.presets }

main =
  Html.text ""
