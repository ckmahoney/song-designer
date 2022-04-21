port module Ports exposing (..)

import Html
import Json.Encode as Encode

port setStorage : (String, Encode.Value) -> Cmd msg

port scrollTo : String -> Cmd msg

main = Html.text ""
