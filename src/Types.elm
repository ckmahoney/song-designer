module Types exposing (..)


import Html


type alias UUID 
  = Int


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


type alias Voice =
  { id : Int
  , duty : SynthDuty
  , role : SynthRole
  , label : String
  , voice : Int -- not editable by user
  , density : Int
  , complexity : Int
  }


type alias ScoreMeta = 
  { title : String
  , cps : Float 
  , root : Float
  , cpc : Int
  }


type alias ScoreMetaP = 
  { title : Maybe String
  , cps : Maybe Float 
  , root : Maybe Float
  , cpc : Maybe Int
  }


-- Record of all invalid values for a given entry for ScoreMeta
emptyScoreMeta : ScoreMeta
emptyScoreMeta = 
  { title = ""
  , cps = 0.0
  , root = 0.00
  , cpc = 0
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


type alias Ensemble = List Voice
type alias NPresetKit = (String, Ensemble)

type alias Section = (Scope, Ensemble)
type alias SectionP = (Maybe Scope, Maybe Ensemble)

type alias Combo = (Scope, Ensemble)
type alias ComboP = (Maybe Scope, Maybe Ensemble)

type alias Layout = (String, List Combo)

type alias Score 
  = (ScoreMeta, List Combo)

type alias TemplateP
  = (ScoreMetaP, List ComboP)


type alias Template
  = (ScoreMeta, Layout)

type alias NamedEnsemble 
  = (String, Ensemble)

main =
  Html.text ""
