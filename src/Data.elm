module Data exposing (..)


import Types exposing (..)
import Tools
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


rangeScopeSize : (Int, Int)
rangeScopeSize = (0, 8)


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


palette2 = 
  paletteTrioDark ++ paletteTrioLight


palette3 = 
  [ "#B81A0C"
  , "#F24738"
  , "#0CB81A"
  , "#38f247"
  , "#1a0cb8"
  , "#4738f2"
  ]


emptyScope : Scope
emptyScope = 
  Scope -969 "Scope" 1 1 1 1


s1 : Scope
s1 =
  { id = -1
  , label = "verse"
  , cps = 1
  , cpc = 4 
  , root = 0
  , size = 4
  }


s2 : Scope
s2 =
  { id = -2
  , label = "chorus"
  , cps = 2
  , cpc = 3
  , root = 1
  , size = 5
  }


s3 : Scope
s3 =
  { id = -3
  , label = "breakdown"
  , cps = 3
  , cpc = 4
  , root = 4
  , size = 3
  }


p1 : Voice
p1 =
  { id = -1
  , duty = Structure
  , role = Bass
  , label = "Bass"
  , voice = 1
  , density = 1
  , complexity = 1
  }


p2 : Voice
p2 =
  { id = -2
  , duty = Structure
  , role = Kick
  , label = "Kick Drum"
  , voice = 0
  , density = 0
  , complexity = 0
  }


p3 : Voice
p3 =
  { id = -3
  , duty = Expression
  , role = Melody
  , label = "Soaring Melody"
  , voice = 4
  , density = 1
  , complexity = 2
  }


p4 : Voice
p4 =
  { id = -4
  , duty = Structure
  , role = Hat
  , label = "Hat Drum"
  , voice = 6
  , density = 1
  , complexity = 2
  }


presets : Ensemble
presets = [p1,p2,p3,p4]


kitAll : Ensemble
kitAll =
  [ Voice 20 Structure Kick "clock" 1 1 0
  , Voice 21 Structure Perc "clap" 3 2 0
  , Voice 22 Structure Hat "offbeat" 5 3 1
  , Voice 23 Structure Bass "pedal" 5 3 1
  , Voice 24 Structure Chords "hook" 5 3 1
  , Voice 25 Structure Melody "phrase" 5 3 1
  ]


kitBeat : Ensemble
kitBeat =
  [ Voice 30 Structure Kick "clock" 1 1 0
  , Voice 31 Structure Perc "clap" 3 2 0
  , Voice 32 Structure Hat "offbeat" 5 3 1
  ]


kitSynth : Ensemble
kitSynth =
  [ Voice 40 Structure Bass "pedal" 5 3 1
  , Voice 41 Structure Chords "hook" 5 3 1
  , Voice 42 Structure Melody "phrase" 5 3 1
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


sections1 : List Section
sections1 =
  [ (s1, kitBeat)
  , (s2, kitSynth)
  , (s3, kitAll)
  ] 


meta1 : ScoreMeta
meta1 =
  { title = "My Delight"
  , cpc = 4
  , cps = 2.1
  , root = 1.65
  }


metaP1 =
  { title = Just "metaTitle"
  , cps = Just 2.3
  , root = Just 33
  , cpc = Just 4
  }


metaP2 =
  { title = Just "Smoothly"
  , cps = Just 3.1
  , root = Just 29
  , cpc = Just 3
  }


template1 = 
  (metaP1, [comboP1, comboP3])


template2 =
  (metaP2, [comboP2, comboP4])


coreTemplates : List Template
coreTemplates =
  [ template1
  , template2
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


findIndex = 
  Tools.findIndex

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


v1 : Scope
v1 =
  { id = -11
  , label = "verse"
  , cps = 2
  , cpc = 4 
  , root = 30
  , size = 1
  }

v2 : Scope
v2 =
  { id = -13
  , label = "verse 2"
  , cps = 4
  , cpc = 8
  , root = 45
  , size = 2
  }


c1 =
  { id = -12
  , label = "chorus"
  , cps = 2
  , cpc = 4 
  , root = 42
  , size = 1
  }



c2 =
  { id = -14
  , label = "chorus 2"
  , cps = 4
  , cpc = 4 
  , root = 35
  , size = 2
  }


layout1 : List Scope
layout1 = [s1, s2, s3]


layout2 : List Scope
layout2 = [ s2, s2, s3, s1]


layout3 : List Scope
layout3 =
  [ v1, v2, c1, c2 ]


coreScopes =
  [ layout1
  , layout2
  , layout3
  ] 


coreLayoutTitles =
  [ "Verse 1", "Verse 2", "Chorus 1", "Chorus 2"]


coreLayouts : List Layout
coreLayouts =
  List.map2 (\a b -> (a, b)) coreLayoutTitles coreScopes


kitVC1 : Ensemble
kitVC1 =
  [ Voice 0 Structure Kick "clock" 1 1 0
  , Voice 1 Structure Perc "clap" 3 2 0
  , Voice 3 Structure Bass "clap" 3 3 1
  , Voice 4 Structure Chords "hook" 5 3 1
  , Voice 5 Expression Melody "phrase" 5 3 1
  ]



kitVC2 : Ensemble
kitVC2 =
  [ Voice 10 Structure Hat "clock" 1 1 0
  , Voice 11 Structure Hat "clap" 3 2 0
  , Voice 12 Expression Hat "clap" 3 2 0
  , Voice 13 Structure Bass "clap" 3 3 1
  , Voice 14 Structure Melody "hook" 5 3 1
  , Voice 15 Structure Chords "phrase" 5 3 1
  ]


scoreVerseChorus : List (Scope, Ensemble)
scoreVerseChorus =
  [ (v1, kitVC1)
  , (c1, kitVC1)
  , (v1, kitVC1)
  , (c1, kitVC1)
  , (v1, kitVC2)
  , (c1, kitVC2)
  ]


ens1 =
  ("Beat", kitBeat)


ens2 = 
  ("Synth", kitSynth)


ens3 = 
  ("Everything", kitAll)


coreNamedEnsembles : List NamedEnsemble
coreNamedEnsembles =
  [ ens1, ens2, ens3 ] 



combo1 =
  (v1, ens2)


combo2 =
  (c1,ens2)

combo3 : Combo
combo3 =
  (v1,ens3)  


comboP1 = 
  (Nothing, Nothing)

comboP2 = 
  (Just v1, Nothing)

comboP3 = 
  (Nothing, Just ens2)

comboP4 =
  (Just v1, Just ens2)


combos : List Combo
combos =
  [ combo1, combo2, combo3 ]

coreScores : List Score
coreScores =
  [ ( ScoreMeta "Simple binary" 4 2.1 35
    , [ combo1
      , combo2
      ] )
  , ( ScoreMeta "Simple Ternary" 4 1.9 29
    , [ combo1
      , combo2
      , combo1
      ] )
  , ( ScoreMeta "Song Form" 4 1.6 28
    , [ combo1
      , combo2
      , combo3
      , (c1,ens3)
      , combo1
      , (c1,ens1)
      , (v1,ens1)
      , (c2,ens3)
      ] )
  , ( ScoreMeta "Another Song Form" 3 2.5 33
    , [ (v1,ens1)
      , combo1
      , combo3
      , combo2
      , (v2,ens2)
      , (v2,ens2)
      , (v2,ens3)
      , (c1,ens3)
      ] )
  ] 



-- scores1 : List Score



main = 
  Html.text ""
