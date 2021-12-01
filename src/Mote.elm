module Mote exposing (Mote,view)

import Html exposing (..)
import Html.Attributes exposing (..)

type alias Mote =
  { freq : Float
  , amp : Float
  , dur : Float
  }


numVoices = 6


duration : Int -> Float -> Html msg
duration cpc dur =
  div [class "column is-one-third has-background-info" ]
    [ text <| String.fromFloat dur 
    ]


frequency : Float -> Float -> Html msg
frequency root freq =
  let
    height = 10 * logBase root freq
  in 
  div [class "column is-one-third has-background-danger" ]
    [ div [ class "has-background-info"
          , style "height"  <| String.fromInt <| truncate height ] []
    , text <| String.fromFloat freq 
    ]
    -- , text <| String.fromInt <| truncate height  ]


amplitude : Float -> Html msg
amplitude amp =
  let
    height = (String.fromFloat (amp * 100))
  in 
  div [class "mote-amplitude column is-one-third has-background-warning" ]
    [ div [ class "has-background-info"
          , style "height" (height ++ "%") ] []
    ]


view : Float -> Int -> Mote -> Html msg
view root cpc ({freq, dur, amp} as mote) =
  div [ class "columns box" ]
   [ amplitude amp
   , frequency root freq
   , duration cpc dur
   ]  


main = text ""
