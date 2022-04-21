module Chords exposing (..)

import Html 
import Tools

type Quality 
  = O -- overtones
  | U -- undertones
  | D -- domintants


type alias Voicing = (Int, Quality)


chromatics : List String
chromatics = ["C", "G", "D", "A", "E", "B", "F#", "Db", "Ab", "Eb", "Bb", "F"]


qualities : List Quality
qualities = [O, U, D]


-- The chord positions within a Tonal Cloud
positions : List Int
positions = [1, 2, 3]


new : Voicing
new = (2, O)


index : Int -> Int
index position =
  let max = List.length chromatics - 1 in
  if position > max then modBy max position
  else let p = abs position in 
  index <| max - p


qLabel : Quality -> String
qLabel q = 
  case q of 
    O -> "Major"
    U -> "Minor"
    D -> "Ambiguous"


flip : Voicing -> Voicing
flip voicing = 
  let num = Tuple.first voicing in 
  case Tuple.second voicing of 
    O ->
      (num, U)

    U ->
      (num, U)
  
    D -> 
      (num + 6, D)


tonalCloud : List Voicing
tonalCloud = 
  List.concatMap (\q -> 
    case q of 
      D -> [(0, D)]
      _ -> List.map (\num -> (num, q)) positions
  ) qualities


neighbors : Voicing -> List Voicing
neighbors voicing =
  List.filter (\q -> q /= Tuple.second voicing) qualities
  |> List.map (\q -> (Tuple.first voicing, q))


gotoOptions : Voicing -> List Voicing
gotoOptions voicing = 
  case voicing of 
    (i, D) -> 
      tonalCloud 
    _ -> 
      flip voicing :: (neighbors voicing)

read : Voicing -> Html.Html msg
read (num, q) = 
  Html.div [] 
    [ Html.p [] [ Html.text <| Tools.getOr num chromatics "_"]
    , Html.p [] [ Html.text <| qLabel q ]
    ] 
 
main = Html.text ""
      
