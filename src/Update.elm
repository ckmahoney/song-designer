module Update exposing (..)

import Html
import Random
import Debug exposing (log)
import Array 
import Time
import Browser



import Types as T
import Data as D


type UpdateMsg
  = Tick Time.Posix
  | UpdateSynthRole T.SynthPreset T.SynthRole  
  | NewID Int
  | RequestPreset
  | NewPreset Int
  | AddPreset T.SynthPreset
  | UpdatePreset (Maybe T.SynthPreset)
  | KillPreset T.SynthPreset
  | ChangeSelection (Maybe T.SynthPreset)
  | UpdateDensity Int
  | UpdateComplexity Int



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


uuid : Random.Generator Int
uuid = 
  Random.int 1 100000000000000000000


createPreset : Int -> T.SynthPreset
createPreset id_ =   
  let 
    ref = D.p1
  in
  { ref | id = id_ }


remove : Maybe a -> List a -> List a
remove x xs =
  case x of 
   Nothing ->
     xs

   Just jx ->
     List.filter (\a -> not (jx == a)) xs

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


update : UpdateMsg -> T.State T.SynthPreset -> (T.State T.SynthPreset, Cmd UpdateMsg)
update msg model =
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
                 , title = (Tuple.second <| D.roleLabel r) }
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

main =
  Html.text ""
