module Encoders exposing (..)
-- Common data types encoded to JSON 

import Defs.Configs as Conf
import Defs.Types exposing (..)
import Defs.Data
import Json.Encode as Encode
import Html exposing (text)
import Url.Builder

encodeScope : Scope -> Encode.Value
encodeScope {label, cps, cpc, root, size} =
  Encode.object
    [ ("label", Encode.string label)
    , ("cps", Encode.float cps)
    , ("root", Encode.float <| toFloat root)
    , ("cpc", Encode.int cpc)
    , ("size", Encode.int size)
    ]


encodeVoice : Voice -> Encode.Value
encodeVoice {duty, role, label, voice, density, complexity} =
  Encode.object
    [ ("duty", Encode.string <| Data.dutyString duty)
    , ("role", Encode.string <| Data.roleId role)
    , ("label", Encode.string label)
    , ("voice", Encode.int voice)
    , ("density", Encode.int density)
    , ("complexity", Encode.int complexity)
    ]


encodeEnsemble : Ensemble -> Encode.Value
encodeEnsemble  =
  Encode.list encodeVoice 


encodeScoreMeta : ScoreMeta -> Encode.Value
encodeScoreMeta {title, cps, root, cpc} =
  Encode.object
    [ ("title", Encode.string title)
    , ("cps", Encode.float cps)
    , ("root", Encode.float root)
    , ("cpc", Encode.int cpc)
    ]


encodeCombo : Combo -> Encode.Value
encodeCombo (scope, ensemble) =
  Encode.object
    [ ("scope", encodeScope scope) 
    , ("ensemble", encodeEnsemble ensemble)
    ]


encodeLayout : Layout -> Encode.Value
encodeLayout  combos =
  Encode.object
    [ ("title", Encode.string "ensemble")
    , ("combos", Encode.list encodeCombo combos)
    ]

encodeReqTrack : String -> String -> Template -> Encode.Value
encodeReqTrack email uuid template =
  Encode.object
    [ ("meta", encodeScoreMeta <| Tuple.first template)
    , ("layout", encodeLayout <| Tuple.second template)
    , ("email", Encode.string email)
    , ("uuid", Encode.string uuid)
    ]


encodeMember : GhostMember -> Encode.Value
encodeMember member =
  Encode.object
    [ ("name", Encode.string member.name)
    , ("email", Encode.string member.email)
    , ("uuid", Encode.string member.uuid)
    ]


encodeReqRegister : Conf.RegEntry -> Encode.Value
encodeReqRegister {email, name, trackIDs, requestSrc}  =
  Encode.object 
    [ ("email", Encode.string email)
    , ("name" , Encode.string name)
    , ("trackIDs", Encode.list Encode.int trackIDs)
    , ("requestSrc", Encode.string requestSrc)
    ]


queryReqLead : String -> String -> List Int -> String -> String
queryReqLead email name trackIDs requestSrc =
  let  
    ids : String
    ids = List.foldl String.append ""  <| List.intersperse ","  <| List.map String.fromInt trackIDs
  in 
  Url.Builder.relative [Conf.leadUrl]
    <| List.map2 Url.Builder.string 
       [ "email", "name", "trackIDs", "requestSrc" ]
       [ email, name,"[" ++  ids ++ "]", requestSrc ] 


ints : List Int -> Encode.Value
ints ids =
  Encode.list Encode.int ids


main = text ""
