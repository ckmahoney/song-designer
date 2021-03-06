module Tools exposing (..)

import Html
import Array
import Defs.Types as Types exposing (..)
import Defs.Data as Data


cpsToBPM : Float -> Float
cpsToBPM cps =
  60.0 * cps


sizeToCycles: Int -> Int -> Int
sizeToCycles cpc size =
 cpc * (2^size)


duration : Int -> Float -> Int -> Float
duration cpc cps size  =
  cps * (toFloat (sizeToCycles cpc size))


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


getTimes : List ScopeFloat -> List (Float, Int)  -- (cps, dur)
getTimes scopes  =
  List.map (\{cps,cpc,size} -> (cps, sizeToCycles cpc size)) scopes


layoutDuration : List ScopeFloat -> Float
layoutDuration scopes =
  List.foldl (\(cps, nCycles) total -> total + (cps * toFloat nCycles)) 0 (getTimes scopes)


chromaticKey : Int -> Float
chromaticKey index =
  getOr index Data.chromaticRoots 15.0


conj x xs =
  List.append xs [x]


get : Int -> List a -> Maybe a 
get index list =
    Array.get index 
     <| Array.fromList list

getOr : Int -> List a -> a ->  a 
getOr index list default =
    Maybe.withDefault default 
     <| Array.get index 
     <| Array.fromList list


-- Returns the index of an element in list, or -1
findIndex : a -> List a -> Int
findIndex x xs =
  let 
    i = List.indexedMap (\j a -> if x == a then j else -1) xs
      |> List.filter (\j -> not (j == -1))
      |> List.head
  in 
  case i of
  Nothing -> 
    -1
  
  Just y ->
    y


find : (a -> Bool) -> List a -> a -> a
find search list default =
  case List.head <| List.filter search list of
    Nothing -> 
      default

    Just found ->
      found


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
  Array.set (findIndex prev xs) next arr
  |> Array.toList


unMaybe : List (Maybe a) -> a -> List a
unMaybe xs def =
  xs |>
  List.filter (\may -> 
    case may of 
      Nothing -> False
      _ -> True)
  |> List.map (\may ->
    case may of
      Nothing -> 
        def
      Just x ->
        x)


isNothing : Maybe a -> Bool
isNothing x =
  case x of 
    Nothing -> True
    Just y -> False


replaceAt : Int -> a -> List a -> List a
replaceAt index el els =
  Array.set index el (Array.fromList els)
  |> Array.toList


insertAt : Int -> a -> List a -> List a
insertAt index el els =
  List.concat
   [ List.take index els
   , List.singleton el
   , List.drop index els
   ] 


toggleElement : a -> List a -> List a
toggleElement x xs =
  let
    position = findIndex x xs
  in 
  if -1 == position then
    x :: xs
  else 
    remove (Just x) xs

main = Html.text ""
