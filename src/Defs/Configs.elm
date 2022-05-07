module Configs exposing (..)

import Data
import Types exposing (GhostMember)
import Html exposing (text)
import File.Download as Download
import Url.Builder as Url

type alias RegEntry = 
  { email : String
  , name : String
  , trackIDs : List Int
  , requestSrc : String
  }


devMode = False
useLocal = False

 
regUrl : String
regUrl = 
  "members/api/send-magic-link/"


leadUrl : String
leadUrl = 
  "user/interested"


regData : String -> String -> List Int -> RegEntry
regData email name ids  =
  RegEntry email name ids "minimaker"


hostname = 
  if useLocal then 
    "http://localhost:3000"
  else if devMode then 
    "https://recordrecorder.com"
  else 
    "https://synthony.app"

selfUrl : String -> String
selfUrl endpoint =
  hostname ++ "/" ++ endpoint


apiUrl : String -> String 
apiUrl endpoint =
  Url.crossOrigin hostname [ endpoint ] []


download : String -> Cmd msg
download url =
  let
     target = if "http" == (String.left 4 url) then url 
       else hostname ++ url
  in
  Download.url target


-- used to demo the MiniMaker on the frontpage
anonMember : GhostMember
anonMember =
  if devMode then 
    Data.testMember
  else 
    Data.anonMember

cardWidth : Float
cardWidth =
  240

main = text ""
