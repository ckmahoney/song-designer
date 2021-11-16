module Data exposing (..)


import Types exposing (..)
import Array
import Html


-- child module of Types
-- providing helpers for humanizing as data


minDensity : Int
minDensity = 1


maxDensity : Int
maxDensity = 4


minComplexity : Int
minComplexity = 0

maxComplexity : Int
maxComplexity = 6


rangeDensity : (Int, Int)
rangeDensity = (minDensity, maxDensity)


rangeComplexity : (Int, Int)
rangeComplexity = (minComplexity, maxComplexity)


defaultCPC = 4

rangeCPC : (Int, Int)
rangeCPC = (1, 7)


defaultCPS = 1


rangeCPS : (Float, Float)
rangeCPS = (0.5, 4)
rangeStep = 0.05

rangeCompoSize : (Int, Int)
rangeCompoSize = (0, 8)

rangeRoot : (Float, Float)
rangeRoot  = (16.0, 16.0 * 2)

chromaticRoots =
  let 
    root = Tuple.first rangeRoot
    octave = Tuple.second rangeRoot - root
    nSteps = 12
    step = octave / nSteps
  in
  List.map (\i -> root + (step * (toFloat i))) <| List.range 0 11


sharps =
  [ "C"
  , "C#"
  , "D"
  , "D#"
  , "E"
  , "F"
  , "F#"
  , "G"
  , "G#"
  , "A"
  , "A#"
  , "B"
  ] 


flats = 
  [ "C"
  , "Db"
  , "D"
  , "Eb"
  , "E"
  , "F"
  , "Gb"
  , "G"
  , "Ab"
  , "A"
  , "Bb"
  , "B"
  ]
  

indexedSharps
  = List.map2 Tuple.pair (List.range 0 11) sharps


indexedFlats
  = List.map2 Tuple.pair (List.range 0 11) flats


paletteTrio : Palette
paletteTrio =
  [ "#E5210F"
  , "#0FE521"
  , "#210FE5"
  ]


paletteTrioDark : Palette
paletteTrioDark =
  [ "#B81A0C"
  , "#0CB81A"
  , "#1A0CB8"
  ]

paletteTrioLight : Palette
paletteTrioLight =
  [ "#F24738"
  , "#38F247"
  , "#4738F2"
  ]

palette =
  [ "#ffa822"
  , "#227bff"
  , "#ff1900"
  , "#00e5ff"
  , "#11ff00"
  , "#ee00ff"
  ]

palette3 = 
  [ "#B81A0C"
  , "#F24738"
  , "#0CB81A"
  , "#38f247"
  , "#1a0cb8"
  , "#4738f2"
  ]


s1 : Compo
s1 =
  { id = -1
  , label = "verse"
  , cps = 1
  , cpc = 4 
  , nCycles = 64
  , root = 0
  , size = 4
  }


s2 : Compo
s2 =
  { id = -2
  , label = "chorus"
  , cps = 2
  , cpc = 3
  , nCycles = 96
  , root = 1
  , size = 5
  }


s3 : Compo
s3 =
  { id = -3
  , label = "breakdown"
  , cps = 3
  , cpc = 4
  , nCycles = 256
  , root = 4
  , size = 3
  }


layout1 : List Compo
layout1 = [s1, s2, s3]


p1 : SynthPreset
p1 =
  { id = -1
  , duty = Structure
  , role = Bass
  , title = "Bass"
  , voice = 1
  , density = 1
  , complexity = 1
  }


p2 : SynthPreset
p2 =
  { id = -2
  , duty = Structure
  , role = Kick
  , title = "Kick Drum"
  , voice = 0
  , density = 0
  , complexity = 0
  }


p3 : SynthPreset
p3 =
  { id = -3
  , duty = Expression
  , role = Melody
  , title = "Soaring Melody"
  , voice = 4
  , density = 1
  , complexity = 2
  }


p4 : SynthPreset
p4 =
  { id = -4
  , duty = Structure
  , role = Hat
  , title = "Hat Drum"
  , voice = 6
  , density = 1
  , complexity = 2
  }




presets : PresetKit
presets = [p1,p2,p3,p4]



kitAll : PresetKit
kitAll =
  [ SynthPreset 0 Structure Kick "clock" 1 1 0
  , SynthPreset 1 Structure Perc "clap" 3 2 0
  , SynthPreset 2 Structure Hat "offbeat" 5 3 1
  , SynthPreset 3 Structure Bass "pedal" 5 3 1
  , SynthPreset 4 Structure Chords "hook" 5 3 1
  , SynthPreset 5 Structure Melody "phrase" 5 3 1
  ]


kitBeat : PresetKit
kitBeat =
  [ SynthPreset 0 Structure Kick "clock" 1 1 0
  , SynthPreset 1 Structure Perc "clap" 3 2 0
  , SynthPreset 2 Structure Hat "offbeat" 5 3 1
  ]


kitSynth : PresetKit
kitSynth =
  [ SynthPreset 0 Structure Bass "pedal" 5 3 1
  , SynthPreset 1 Structure Chords "hook" 5 3 1
  , SynthPreset 2 Structure Melody "phrase" 5 3 1
  ]


kits : List NPresetKit
kits = 
  [ ("The beats", kitBeat)
  , ("One of Everything", kitAll) 
  , ("The synths", kitSynth)
  ]


allKits : List Ensemble
allKits =
  [ kitBeat
  , kitAll
  , kitSynth
  ]


score1 : Score
score1 =
  [ (s1, kitBeat)
  , (s2, kitSynth)
  , (s3, kitAll)
  ] 



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


get : Int -> List a -> Maybe a
get i xs =
  if (List.length xs < i) then
    Nothing
  else 
    let 
      tmp = Array.fromList xs
    in
    Array.get i tmp


-- Returns the index of an element in list, or -1
findIndex : a -> List a -> Int
findIndex x xs =
  let 
    i = List.indexedMap (\j a -> if x == a then j else -1) xs
      |> List.filter (\j -> not (j == -1))
      |> List.head
  in 
  case i of
  Nothing -> 
    -1
  
  Just y ->
    y


-- Given an item, gets the related index of the next item
relate : a -> List a -> List b -> Maybe b
relate x xs ys = 
  get (findIndex x xs) ys


roleDescription : SynthRole -> String
roleDescription role =
  case role of 
    Kick ->
      "The low drum in a beat. Usually sounds deep or punchy, but can also be smooth and clean."

    Perc ->
      "Any percussive element that is not a kick or hat. Claps, snare, toms, cowbell, Pringles... the list goes on."

    Hat ->
      "The high drum in a beat. Usually sounds bright or sharp, and played with short notes. Great for complex rhythms that sound clean."

    Bass ->
      "The lowest instrument voice. It is the most important for setting up harmony."

    Chords ->
      "Instruments that play more than one note. It tells a story through changing harmony."

    Melody ->
      "The most free and expressive instrument. This has the most variety of results."


roleColor : SynthRole -> String
roleColor role =
  case (relate role roles palette) of
    Nothing ->
      ""
    Just color ->
      color


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
