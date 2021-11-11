module Types exposing (..)


import Html


type alias UUID 
  = Int


type Msg
  = ButtonClick


type Texture
  = Density
  | Complexity


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


type alias Palette = List String


type alias SynthLabel
  = (SynthRole, String)

type alias SynthPreset =
  { id : Int
  , duty : SynthDuty
  , role : SynthRole
  , title : String
  , voice : Int
  , density : Int
  , complexity : Int
  }


type alias PresetKit = List SynthPreset

type alias NPresetKit = (String, PresetKit)

type alias ScoreMeta =
  { cps : Float
  , root : Float
  , nCycles : Int
  }

type alias State a =
  { time : Int
  , current : Maybe a
  , presets : List SynthPreset
  }

main =
  Html.text ""
