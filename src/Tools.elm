module Tools exposing (..)

import Html
import Array
import Types exposing (..)
import Data 

sizeToCycles: Int -> Int -> Int
sizeToCycles cpc size =
 cpc * (2^size)


duration : Int -> Float -> Int -> Float
duration cpc cps size  =
  cps * (toFloat (sizeToCycles cpc size))


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
    


replaceAt : Int -> a -> List a -> List a
replaceAt index el els =
  Array.set index el (Array.fromList els)
  |> Array.toList


main = Html.text ""
