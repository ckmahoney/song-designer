module Update exposing (..)

import Html
import Random
import Debug exposing (log)
import Array 

import Types as T
import Data as D


type UpdateMsg
  = UpdateSynthRole T.SynthPreset T.SynthRole  
  | NewID Int
  | NewPreset Int
  | SavePreset T.SynthPreset
  | KillPreset T.SynthPreset
  | ChangeSelection T.SynthPreset
  | UpdateDensity Int
  | UpdateComplexity Int


conj x xs =
  List.reverse <| x :: xs


rint : Random.Generator Int
rint =
  Random.int 1 1000


genID : Cmd UpdateMsg
genID = 
  Random.generate NewID rint

genPreset : Cmd UpdateMsg
genPreset = 
  Random.generate NewPreset rint


uuid : Random.Generator Int
uuid = 
  Random.int 1 100000000000000000000


createPreset : Int -> T.SynthPreset
createPreset id_ =   
  let 
    ref = D.p1
  in
  { ref | id = id_ }


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
 
      NewPreset id ->
       let
         next = createPreset id
       in      
       noCmd <| ({ model | current = next, presets = next :: model.presets })      

      SavePreset preset ->
       ({ model | presets = conj preset model.presets }, genID)

      KillPreset preset ->
       ({ model | presets = remove preset model.presets }, Cmd.none)
  
      ChangeSelection preset ->
        noCmd <| { model | current = preset }

      UpdateSynthRole preset r ->
        let 
          prev = model.current
          next = { prev | role = r, title = (Tuple.second <| D.roleLabel r)}
          index = D.findIndex prev model.presets
          presets = Array.toList <| Array.set index next <| Array.fromList model.presets
        in 
        noCmd <|
          { current = next
          , presets = presets }

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
