module Data exposing (..)

import Types exposing (..)
import Array
import Html


-- child module of Types
-- providing helpers for humanizing as data


minDensity : Int
minDensity = 1


maxDensity : Int
maxDensity = 3


minComplexity : Int
minComplexity = 1

maxComplexity : Int
maxComplexity = 3


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


fitRoot : Float -> Float
fitRoot root = 
  let
    (min, max) = rangeRoot
  in 
  if root <= max && root >= min then 
    root
  else if  root > max then 
    fitRoot (root / 2)
  else 
    fitRoot (root * 2)


chromaticRoots : List Float
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
  Scope -969 "New Scope" 1.0 4 0 2


emptyEnsemble : Ensemble
emptyEnsemble = 
  []


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
  , density = 1
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
  , density = 2
  , complexity = 2
  }


presets : Ensemble
presets = [p1,p2,p3,p4]


kitAll : Ensemble
kitAll =
  [ Voice 20 Structure Kick "clock" 1 1 1
  , Voice 21 Structure Perc "clap" 3 2 1
  , Voice 22 Structure Hat "offbeat" 5 3 1
  , Voice 23 Structure Bass "pedal" 5 3 1
  , Voice 24 Structure Chords "hook" 5 3 1
  , Voice 25 Structure Melody "phrase" 5 3 1
  ]


kitBeat : Ensemble
kitBeat =
  [ Voice 30 Structure Kick "clock" 1 1 1
  , Voice 31 Structure Perc "clap" 3 2 1
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
  , root = (1.65 * 16 * 2)
  -- , root = 1.65
  }


emptyVoice : Voice
emptyVoice =
  { id = -1
  , duty = Structure
  , role = Kick
  , label = ""
  , voice = 0
  , density = 1
  , complexity = 1
  } 


emptyMetaP =
  { title = Just ""
  , cpc = Just 1
  , cps = Just 1
  , root = Just 1
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


emptyTemplate : Template
emptyTemplate = 
  (emptyScoreMeta, emptyLayout)


-- someTemplate : Template
-- someTemplate = 
  -- (meta1, someLayout)


-- coreTemplates : List TemplateP
-- coreTemplates =
  -- [ template1
  -- , template2
  -- ] 



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


roleId : SynthRole -> String
roleId role =
  Tuple.first <| roleLabel role


get : Int -> List a -> Maybe a
get i xs =
  if (List.length xs < i) then
    Nothing
  else 
    let 
      tmp = Array.fromList xs
    in
    Array.get i tmp

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
      ("complexity", "Group")


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


dutyString : SynthDuty -> String
dutyString duty =
  case duty of 
    Structure -> "structure"
    Expression -> "expression"


v1 : Scope
v1 =
  { id = -11
  , label = "verse"
  , cps = 14/10
  , cpc = 4 
  , root = 5
  , size = 3
  }


v2 : Scope
v2 =
  { id = -13
  , label = "verse 2"
  , cps = 14/10
  , cpc = 8
  , root = 2
  , size = 3
  }


c1 =
  { id = -12
  , label = "chorus"
  , cps = 14/10
  , cpc = 4 
  , root = 7
  , size = 4
  }


c2 =
  { id = -14
  , label = "chorus 2"
  , cps = 14/10
  , cpc = 4 
  , root = 2
  , size = 4
  }


emptyLayout : Layout
emptyLayout = 
  []


dCPS = 1.5
dCPC = 4
dRoot = 32

sIntro : Scope
sIntro = 
  Scope -9690 "Intro" dCPS dCPC dRoot 1

sOutro : Scope
sOutro = 
  Scope -9690 "Outro" dCPS dCPC dRoot 1


demoIntro : Combo
demoIntro =
  (sIntro, kitBeat)

demoOutro : Combo
demoOutro =
  (sOutro, kitBeat)

sVerse : Scope
sVerse =
  Scope -9691 "Verse" dCPS dCPC dRoot 3

sChorus : Scope
sChorus =
  Scope -9692 "Chorus" dCPS dCPC dRoot 2

demoVerse : Combo
demoVerse = 
  (sVerse, kitSynth)

demoChorus : Combo
demoChorus = 
  (sChorus, kitAll)

demoLayout : Layout
demoLayout =
  [demoIntro, demoVerse, demoChorus, demoVerse, demoChorus, demoChorus, demoOutro]

demoTemplate : Template
demoTemplate =
 let
  m = scoreMetaFast
 in
  ({ m | title = "Verse-Chorus song with intro and outro" }, demoLayout)

emptyCombo : Combo
emptyCombo = 
  (emptyScope, [])


emptyComboP : ComboP
emptyComboP = 
  (Nothing, Nothing)


emptyMember : GhostMember
emptyMember =
  GhostMember "" "" "" "" "" False False []

testMember : GhostMember
testMember =
  GhostMember "263bb49a-a3e7-471e-a945-74570b25fd53" "test-user" "test-user mike" "" "demo@mail.com" False False []


scopes1 : List Scope
scopes1 = [s1, s2, s3]


scopes2 : List Scope
scopes2 = [ s2, s2, s3, s1]


scopes3 : List Scope
scopes3 =
  [ v1, v2, c1, c2 ]


coreScopes =
  [ scopes1
  , scopes2
  , scopes3
  ] 


coreLayoutTitles =
  [ "Verse 1", "Verse 2", "Chorus 1", "Chorus 2"]


kitVC1 : Ensemble
kitVC1 =
  [ Voice 0 Structure Kick "clock" 1 1 1
  , Voice 1 Structure Perc "clap" 3 2 1
  , Voice 3 Structure Bass "clap" 3 3 1
  , Voice 4 Structure Chords "hook" 5 3 1
  , Voice 5 Expression Melody "phrase" 5 3 1
  ]



kitVC2 : Ensemble
kitVC2 =
  [ Voice 10 Structure Hat "clock" 1 1 1
  , Voice 11 Structure Hat "clap" 3 2 1
  , Voice 12 Expression Hat "clap" 3 2 1
  , Voice 13 Structure Bass "clap" 3 3 1
  , Voice 14 Structure Melody "hook" 5 3 1
  , Voice 15 Structure Chords "phrase" 5 3 1
  ]


scoreVerseChorus : List Combo
scoreVerseChorus =
  [ (v1, kitVC1)
  , (c1, kitVC1)
  , (v1, kitVC1)
  , (c1, kitVC1)
  , (v1, kitVC2)
  , (c1, kitVC2)
  ]


templateVerseChorus : Template
templateVerseChorus =
  (scoreMetaFast, scoreVerseChorus)


templateTernary : Template
templateTernary =  
  let
   a : ScoreMeta
   a = { title = "Three's Company"
       , cps = 2.8
       , root = 8
       , cpc = 4
       }
   b : Layout 
   b = [ ({ id = 0, label = "intro", size = 2, cpc = 8, root = 0, cps = 1.0 }, kitVC1)
          , ({ id = 1, label = "partA", size = 4, cpc = 4, root = 2, cps = 1.0 }, kitVC2)
          , ({ id = 2, label = "partB", size = 8, cpc = 8, root = 9, cps = 1.0 }, kitVC2)
          , ({ id = 3, label = "partA", size = 4, cpc = 4, root = 2, cps = 1.0 }, kitVC1)
          , ({ id = 4, label = "outro", size = 2, cpc = 8, root = 0, cps = 1.0 }, kitVC1)
          ]

  in
  (a,b)



templateABAC : Template
templateABAC =  
  let
   a : ScoreMeta
   a = { title = "Fours a Crowd"
       , cps = 2.8
       , root = 8
       , cpc = 4
       }
   b : Layout 
   b = [ ({ id = 0, label = "intro", size = 2, cpc = 4, root = 0, cps = 1.0 }, kitVC1)
          , ({ id = 1, label = "partA", size = 4, cpc = 4, root = 2, cps = 1.0 }, kitVC2)
          , ({ id = 2, label = "partB", size = 8, cpc = 8, root = 9, cps = 1.0 }, kitVC2)
          , ({ id = 3, label = "partA", size = 4, cpc = 4, root = 2, cps = 1.0 }, kitVC1)
          , ({ id = 5, label = "partC", size = 16, cpc = 8, root = 5, cps = 1.0 }, kitVC2)
          , ({ id = 4, label = "outro", size = 2, cpc = 4, root = 0, cps = 1.0 }, kitVC1)
          ]

  in
  (a,b)


templateAB : Template
templateAB =  
  let
   a : ScoreMeta
   a = { title = "Simple Compliments"
       , cps = 2.8
       , root = 8
       , cpc = 4
       }
   b : Layout 
   b = [ ({ id = 0, label = "intro", size = 2, cpc = 4, root = 0, cps = 1.0 }, kitVC1)
          , ({ id = 1, label = "partA", size = 8, cpc = 8, root = 3, cps = 1.0 }, kitVC2)
          , ({ id = 2, label = "partB", size = 8, cpc = 8, root = 10, cps = 1.0 }, kitVC2)
          , ({ id = 4, label = "outro", size = 2, cpc = 4, root = 0, cps = 1.0 }, kitVC1)
          ]

  in
  (a,b)


ens1 =
  ("Beat", kitBeat)


ens2 = 
  ("Synth", kitSynth)


ens3 = 
  ("Everything", kitAll)


coreEnsembles : List Ensemble
coreEnsembles =
  [ kitBeat, kitSynth, kitAll ] 


combo1 =
  (v1, kitAll)


combo2 =
  (c1,kitSynth)


combo3 : Combo
combo3 =
  (v1,kitAll)  


combos : List Combo
combos =
  [ combo1, combo2, combo3 ]


emptyScore : Score
emptyScore =
  (emptyScoreMeta, [emptyCombo])


emptyScoreMeta =
  ScoreMeta "" 1 1 1


someLayout : Layout
someLayout =
   [combo1, combo2, combo3]

someTemplate : Template
someTemplate =
  ({ scoreMetaLudacris| title ="A sequence of music"}, [combo1, combo2, combo3])


-- verseChorusLayout : Layout
-- verseChorusLayout =
  

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
      , (c1,kitAll)
      , combo1
      , (c1,kitBeat)
      , (v1,kitBeat)
      , (c2,kitAll)
      ] )
  , ( ScoreMeta "Another Song Form" 3 2.5 33
    , [ (v1,kitBeat)
      , combo1
      , combo3
      , combo2
      , (v2,kitSynth)
      , (v2,kitSynth)
      , (v2,kitAll)
      , (c1,kitAll)
      ] )
  ] 


templates = [ templateTernary
             , templateABAC
             , templateAB
             , templateVerseChorus
             ]


scoreMetaT0 : ScoreMeta
scoreMetaT0 =
  ScoreMeta "Fresh <rivers>?!?!" 4 1 32


scoreMetaSlow : ScoreMeta
scoreMetaSlow =
  ScoreMeta "Ballad for " (3/4) 5 8


scoreMetaMed : ScoreMeta
scoreMetaMed =
  ScoreMeta "Walking for " 1.5 7 4

scoreMetaFast : ScoreMeta
scoreMetaFast =
  ScoreMeta "Fast for " 2.5 0 8

scoreMetaLudacris : ScoreMeta
scoreMetaLudacris =
  ScoreMeta "Speed up tiger" 5 1 16

scoreMetaRipley : ScoreMeta
scoreMetaRipley =
  ScoreMeta "believe it or not " 82 11 32


roleName : SynthRole -> String
roleName role = 
  (Tuple.first <| roleLabel role)

main = 
  Html.text ""

