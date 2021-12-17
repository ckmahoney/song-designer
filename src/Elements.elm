module Elements exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)
import Components
import Tools
import Data

useSharps = False

pitches = if useSharps then Data.indexedSharps else Data.indexedFlats


scope : Scope -> Html msg
scope ({label, cps, cpc, root, size} as scope_) =
  Components.card label <|
    div [] 
      [ p [] [ text <| (String.fromInt <| Tools.sizeToCycles cpc size) ++ " beats" ]
      , p [] [ text <| (String.fromFloat <| Tools.duration cpc cps size) ++ " seconds"]
      , p [] [ text <| "Key of " ++ (Tuple.second <| Tools.getOr root pitches (-1, "Unkown")) ]
      ]


main = 
  Html.text ""
