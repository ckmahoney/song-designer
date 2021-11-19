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
import Update as U
import View 
import Router 


type alias User = 
  { username : String
  , token : Bool
  }


joe = 
  { username = "Joe"
  , token = True
  }


initApp : Maybe Int -> (App.Module, Cmd msg)
initApp = 
   App.init


subsScore model =
  Sub.none


mainSongDesigner =
  Browser.element { init = App.init
                  , update = App.update
                  , view = App.view
                  , subscriptions = App.subscriptions
                  }


mainRouter =
  Browser.element { init = Router.init
                  , update = Router.update
                  , view = Router.view
                  , subscriptions = Router.subscriptions
                  }


main = 
  -- mainSongDesigner
  mainRouter
