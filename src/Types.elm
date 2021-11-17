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


type Count 
  = Num Int
  | Zero


type alias Voice =
  { id : Int
  , duty : SynthDuty
  , role : SynthRole
  , label : String
  , voice : Int
  , density : Int
  , complexity : Int
  }


-- type alias PresetKit = List SynthPreset
type alias Ensemble = List Voice


type alias NPresetKit = (String, Ensemble)


type alias ScoreMeta = 
  { title : String
  , cps : Float 
  , root : Float
  , cpc : Int
  , nCycles : Int
  }


-- Record of all invalid values for a given entry for ScoreMeta
emptyScoreMeta : ScoreMeta
emptyScoreMeta = 
  { title = ""
  , cps = 0.0
  , root = 0.00
  , cpc = 0
  , nCycles = 0
  }


type alias Token = Bool


-- a request to build one song from configuration
type alias HTTPData = 
  { user : String
  , token : Maybe Token
  , meta : ScoreMeta
  , score : Score
  }



type alias Scope  =
  { id : Int
  , label : String
  , cps : Float
  , cpc : Int
  , root : Int
  , size : Int
  }


type alias EditState a =
  { time : Int
  , current : Maybe a
  , presets : List a
  }


-- type alias SynthState 
  -- = EditState Voice


type alias VoiceEditor
  = (EditState Voice) 


type alias EnsembleEditor
  = EditState Ensemble


type alias EditLayout  = 
  { time : Int
  , index : Int
  , current : Maybe Scope
  , list : List Scope
  , presets : List Scope
  }


type alias Section = (Scope, Ensemble)
type alias SectionP = Maybe (Scope, Ensemble)


type SectionState
  = Section Scope Ensemble
  | NeedsCompo Ensemble
  | NeedsEnsemble Scope
  | EmptySection


-- a Score represents the ordered set of (Compo, Ensemble) pairs.
type alias Score 
  = List Section


type alias EditScore =
  { time : Int
  , current : Maybe Section
  , ensemble : Maybe Ensemble
  , scope : Maybe Scope
  , cps : Float
  , list : List Section
  , layout : List Scope
  , ensembles : List Ensemble
  }


main =
  Html.text ""


