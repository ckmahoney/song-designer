module Designer.SongDesigner exposing (..)
-- Interface for editing song templates. The most control possible of any editor. 

import Browser
import Html exposing (Html, button, div, text, label, p, input,b)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onInput)


import Defs exposing (ScoreMeta, SynthRole)
import View 
import Elements
import Tools
import Components
import Data

import Editor.SketchEditor

import Json.Decode as Decode

type alias Model = 
  (ScoreMeta, List ScopeFloat)


main = Html.text  ""
