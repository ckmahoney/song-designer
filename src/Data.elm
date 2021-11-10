module Data exposing (..)

import Types exposing (..)
import Html


-- child moodule of Types
-- providing helpers for humanizing as data

p1 : SynthPreset
p1 =
  { id = 0
  , duty = Structure
  , role = Bass
  , title = "Big Bass"
  , voice = 1
  , density = 1
  , complexity = 1
  }

p2 : SynthPreset
p2 =
  { id = 1
  , duty = Structure
  , role = Kick
  , title = "Kick Drum"
  , voice = 0
  , density = 0
  , complexity = 0
  }

p3 : SynthPreset
p3 =
  { id = 2
  , duty = Expression
  , role = Melody
  , title = "Soaring Melody"
  , voice = 4
  , density = 1
  , complexity = 2
  }

p4 : SynthPreset
p4 =
  { id = 3
  , duty = Structure
  , role = Hat
  , title = "Hat Drum"
  , voice = 6
  , density = 1
  , complexity = 2
  }


roles : List SynthRole
roles =
  [Kick, Perc, Hat, Bass, Chords, Melody]


roleLabel : SynthRole -> (String, String)
roleLabel role =
  case role of
    Kick ->
      ("kick", "Deep")
    
    Perc ->
      ("perc", "Hits")
    
    Hat ->
      ("hat", "Groove")
    
    Bass ->
      ("bass", "Bassline")
    
    Chords ->
      ("chords", "Chords")
    
    Melody ->
      ("melody", "Melody")


textureLabel : Texture -> (String, String)
textureLabel texture = 
  case texture of 
    Density ->
      ("density", "Density")

    Complexity ->
      ("complexity", "Complexity")


roleDuty: SynthRole -> SynthDuty
roleDuty role =
  case role of 
    Kick ->
      Structure
    
    Perc ->
      Structure
    
    Hat ->
      Expression
    
    Bass ->
      Structure
    
    Chords ->
      Expression
    
    Melody ->
      Expression
    

main = 
  Html.text ""
