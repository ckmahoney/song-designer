module Types exposing (..)


import Html


type Msg
  = ButtonClick

type SynthDuty
  = Structure
  | Expression

type SynthRole
  = Kick
  | Perc
  | Hat
  | Bass
  | Chords
  | Melody

type alias SynthLabel
  = (SynthRole, String)

type alias SynthPreset =
  { duty : SynthDuty
  , role : SynthRole
  , title : String
  , voice : Int
  , density : Int
  , complexity : Int
  }


type alias ScoreMeta =
  { cps : Float
  , root : Float
  , nCycles : Int
  }

type alias State a =
  { current : a
  , presets : List SynthPreset
  }

main =
  Html.text ""
