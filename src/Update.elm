module Update exposing (..)

import Html
import Random
import Debug
import Array 
import Time
import Browser

import Types as T
import Data as D
import Tools

type UpdateMsg
  = Tick Time.Posix
  | NewID Int
  | RequestPreset
  | NewPreset Int


type alias Meta = String


type alias Everything =
  { title : Meta
  , ensembles : List T.Ensemble
  , scopes : List T.Scope
  , score : List T.Section
  }





ptoInt : Time.Posix -> Int
ptoInt t =
  Time.posixToMillis t


conj =
  Tools.conj


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


remove = 
  Tools.remove

removeAt =
  Tools.removeAt


replace =
  Tools.replace

noCmd : a -> (a, Cmd UpdateMsg)
noCmd x =
  (x, Cmd.none)


reindexScope : T.EditLayout -> T.EditLayout
reindexScope ({ index, current, presets } as model) =
  model


main =
  Html.text ""
