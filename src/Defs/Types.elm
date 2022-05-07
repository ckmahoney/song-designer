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


type Posting
  = Sending
  | Received 
  | Failed String
  | Welcome


type alias Track = 
  { id : Int
  , src : String
  , size : Int
  , duration : Float
  }


type alias TrackMeta = 
  { id : Int
  , account_id : Int
  , filepath : String
  , title : String
  , size_bytes : Int
  , duration_seconds : Float
  }


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

type alias ScopeFloat  =
  { id : Int
  , title : String
  , cps : Float
  , cpc : Int
  , root : Float
  , size : Int
  }


type alias GhostMember =
  { uuid : String
  , firstname : String
  , name : String
  , avatar_image : String
  , email : String
  , subscribed : Bool
  , paid : Bool
  , subscriptions: List String
  } 


type alias WithMember a
  = (GhostMember, a)


type alias Ensemble = List Voice
type alias NPresetKit = (String, Ensemble)

type alias Section = (Scope, Ensemble)
type alias SectionP = (Maybe Scope, Maybe Ensemble)

type alias Combo = (Scope, Ensemble)
type alias ComboP = (Maybe Scope, Maybe Ensemble)


type alias Layout =  List Combo


type alias Score 
  = (ScoreMeta, List Combo)

-- type alias TemplateP
  -- = (ScoreMetaP, List ComboP)


type alias Template
  = (ScoreMeta, Layout)

type alias NamedEnsemble 
  = (String, Ensemble)

type alias WithMessage a =
  { a | message : String }

type alias TrackResponse  =
  { track : TrackMeta
  , message : String
  } 



main =
  Html.text ""

