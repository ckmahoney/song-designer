module Main exposing (main)

import Html exposing (Html, button, div, text, label, p)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Browser
import Dict
import Debug
import Time


import App
import Types as T
import Data exposing (p1, p2, p3, p4)
import View 
import Router 
import Editor.SketchEditor


type alias User = 
  { username : String
  , token : Bool
  }


initApp : Maybe Int -> (App.Module, Cmd msg)
initApp = 
   App.init


subsScore model =
  Sub.none


mainApp =
  Browser.element { init = App.init
                  , update = App.update
                  , view = App.view
                  , subscriptions = App.subscriptions
                  }



main = 
  Router.main

