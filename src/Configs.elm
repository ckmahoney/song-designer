module Configs exposing (..)

import Html exposing (text)
import File.Download as Download
import Url.Builder as Url


hostname = 
  "http://localhost:3000"
 -- "https://synthony.app"


apiUrl : String -> String 
apiUrl endpoint =
  Url.crossOrigin hostname [ endpoint ] []


download : String -> Cmd msg
download url =
  Download.url (hostname ++ url)


main = text ""
