module Defs exposing (..)
-- Updated type definitions. Uses existing types when possible; else defines the new formats here.

import Html exposing (text)
import Types


type alias Family = Types.SynthRole


type Change
  = Key
  | Tempo
  | Cycles

type Changes = List Change


-- Describes the function of a section in a song
type Duty
  = Open
  | Exit
  | Declare
  | Structure
  | Paint
  | Transition (List Change)

-- Duties that do not require parameters
coreDuties : List Duty
coreDuties = 
  [ Open, Exit, Declare, Structure, Paint ]

dutyLabel : Duty -> String
dutyLabel duty = 
  case duty of 
    Open -> "Opening"
    Exit -> "Closing"
    Declare -> "Full Body"
    Structure -> "Structural"
    Paint -> "Atmospheric"
    Transition _ -> "Modulate"

dutyDescr : Duty -> String
dutyDescr duty =
  case duty of 
    Open -> "The opening of a song or section in a song. Creates structure, but not focus."
    Exit -> "The ending of a song or section in a song. Evokes previous elements without bringing in new material."
    Declare -> "An intersting part of the song. Think of it like a verse or chorus - full bodied musical statement."
    Structure -> "A skeletal part of the song. Evokes a blueprint without filling in details. Emphasis on rhythm over harmony."
    Paint -> "A textural part of the song. It can feel abstract and whispy. Emphasizes harmony over rhythm."
    Transition _ -> "A functional part of the song. Changes a fundamental part of the song, such as tempo or key signature."

-- Describes how one section relates to another
type Relation 
  = Unique
  | Source
  | Variation Sketch
  | Clone Sketch
  -- | Connect (Sketch, Sketch)

-- References to relations that may take parametrs.
type RelationRef 
  = JUnique
  | JSource
  | JVariation
  | JClone
  -- | JConnect

coreRelations : List RelationRef
coreRelations =
  -- [ JUnique, JSource, JVariation, JClone, JConnect ]
  [ JUnique, JSource, JVariation, JClone ]


relationLabel : Relation -> String
relationLabel relation = 
  case relation of 
    Unique -> "Unique"
    Source -> "Original source material"
    Variation sketch -> "Variation of a previous section"
    Clone sketch -> "Copy of a previous section"
    -- Connect (sketch1, sketch2) -> "Connector between two sections"

relationLabelJ : RelationRef -> String
relationLabelJ ref =
  case ref of 
    JUnique -> relationLabel Unique
    JSource -> relationLabel Source
    JVariation -> relationLabel <| Variation emptySketch
    JClone -> relationLabel <| Clone emptySketch
    -- JConnect -> relationLabel <| Connect (emptySketch, emptySketch)


relationDescr : Relation -> String
relationDescr relation =
  case relation of 
    Unique -> "A speical section that should get special attention. Beats and melodies in here can not be used anywhere else in the song."
    Source -> "An interesting section that should be used and used again. Like a verse - it's changing a little each time. Can be referenced by Variation and Clone."
    Variation _ -> "Given a section, borrows beats or melodies from that section but changes other aspects of it. Like the verse of a verse-chorus song, it will add unity to the layout. Must reference a Unique or Source."
    Clone _ -> "An exact copy of a song. Like the chorus in a verse-chorus song, it is repeated the same way each time."
    -- Connect _ -> "A bridge between two sections. It gets you from point A to point B, borrowing elements from both sides to make a bridge."


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

emptySketch : Sketch 
emptySketch =
  { label = ""
  , duty = Paint
  , relation = Unique
  , size = 1
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
