module Configs exposing (..)

import Types exposing (GhostMember)
import Html exposing (text)
import File.Download as Download
import Url.Builder as Url

type alias RegEntry = 
  { email : String
  , name : String
  , requestSrc : String
  }


devMode = True

 
regUrl : String
regUrl = 
  "members/api/send-magic-link/"


regData : String -> String -> RegEntry
regData email name =
  RegEntry email name "minimaker"


hostname = 
  if devMode then 
    "http://localhost:3000"
  else 
    "https://synthony.app"


apiUrl : String -> String 
apiUrl endpoint =
  Url.crossOrigin hostname [ endpoint ] []


download : String -> Cmd msg
download url =
  Download.url (hostname ++ url)


-- used to demo the MiniMaker on the frontpage
anonMember : GhostMember
anonMember =
  if devMode then 
    GhostMember "ecadef5f-da0f-41c9-9b7f-7d03d3e9b569" "anon" "test-user sandy" "" "anon@synthony.app" False False []
  else 
    GhostMember "f6bc137f-218b-42c7-8a6f-ae445103d96c" "anon" "test-user mike" "" "anon@synthony.app" False False []



main = text ""