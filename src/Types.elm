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
type alias Ensemble = List SynthPreset


type alias NPresetKit = (String, PresetKit)


type alias Compo  =
  { id : Int
  , label : String
  , cps : Float
  , cpc : Int
  , root : Int
  , nCycles : Int
  , size : Int
  }


type alias SeedCell =
  { id : Int
  , action : String
  , nCycles : Int
  } 


type alias EditState a =
  { time : Int
  , current : Maybe a
  , presets : List a
  }


type alias SynthState 
  = EditState SynthPreset


type alias EditLayout  = 
  { time : Int
  , index : Int
  , current : Maybe Compo
  , presets : List Compo
  }


type alias Section = (Compo, Ensemble)


-- a Score represents the ordered set of (Compo, Ensemble) pairs.
type alias Score 
  = List Section


type alias EditScore =
  { time : Int
  , current : Maybe Section
  , cps : Float
  , list : List Section
  , layout : List Compo
  , ensembles : List Ensemble
  }


main =
  Html.text ""
