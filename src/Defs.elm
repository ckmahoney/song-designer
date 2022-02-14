module Defs exposing (..)
-- Updated type definitions. Uses existing types when possible; else defines the new formats here.

import Html exposing (text)
import Types


type alias Family = Types.SynthRole


-- Describes the function of a section in a song
type Duty
  = Open
  | Exit
  | Declare
  | Structure
  | Paint
  | Transition


dutyLabel : Duty -> String
dutyLabel duty = 
  case duty of 
    Open -> "Opening"
    Exit -> "Closing"
    Declare -> "Full Body"
    Structure -> "Structural"
    Paint -> "Atmospheric"
    Transition -> "Connector"


-- Describes how one section relates to another
type Relation 
  = Unique
  | Source
  | Variation Sketch
  | Clone Sketch
  | Connect (Sketch, Sketch)


relationLabel : Relation -> String
relationLabel relation = 
  case relation of 
    Unique -> "Unique"
    Source -> "Original source material"
    Variation sketch -> "Variation of a previous section"
    Clone sketch -> "Copy of a previous section"
    Connect (sketch1, sketch2) -> "Connector between two sections."

-- Arbitrary values that have greater affect on variation than quality.
type alias TemplateConfig =
  { fundamental : Float
  , cpc : Int
  , cps : Float
  , size : Int
  } 


-- Parameters for a section of music.
type alias Sketch =
  { label : String
  , duty : Duty 
  , relation : Relation
  , size : Int
  }


-- Parameters to describe a synthesizer.
type alias Voice =
  { family : Family
  , width : Int
  , height : Int
  , density : Int
  , complexity : Int
  }


-- The voices used for an scope
type alias Group = List Voice


type alias Section = (Sketch, Group)


main = text ""
