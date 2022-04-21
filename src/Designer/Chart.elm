module Designer.Chart exposing (..)

import Browser
import Html exposing (Html, h1, button, div, text, label, p, input,b)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onInput)

import View 
import Elements
import Tools
import Components
import Data
import Components.Group as Group
import Card as Card

import Json.Decode as Decode

type alias CardGroup = (Group.Model Card.Model)

type alias Model = (Group.State Card.Model, Maybe Card.State)

type Msg cmsg
  = View Model
  | InitCard Card.Model
  | SaveCard 
  | KillCard


type State 
  = Viewing Model
  | EditingCard Model Card.State

type alias DefaultMsg = Msg Card.Msg


someCards : List Card.Model
someCards = 
  [ Card.new
  , Card.new2
  , Card.new3
  ]

new : Model
new = 
  (Group.stateFrom (Group.from someCards), Nothing)


init : Maybe Int -> (State, Cmd msg)
init flags = 
  case flags of 
    _ -> (Viewing new, Cmd.none)



update : DefaultMsg -> State -> (State, Cmd msg)
update msg state = 
  case state of 
    Viewing group -> 
      case msg of 
        InitCard card -> 
          let
             cards = Tuple.second group
             index = Tools.findIndex card cards
          in 
          (EditingCard (Just index, cards) Card.initState, Cmd.none)
        _ -> (state, Cmd.none)
    -- Editing model -> (EditingCard model Card.initState, Cmd.none)
    EditingCard orig cardState -> (state, Cmd.none)

  

-- deferM2 : Card.State -> CardMsg msg2 -> Card.State
-- deferM2 state msg =
  -- Card.update msg state

viewCard : Card.State -> msg -> msg -> msg -> Html msg
viewCard state start save cancel  =
  Card.viewSlim state start save cancel


view : State -> msg -> msg -> msg -> msg -> Html msg
view state start save cancel selectCard = 
  case state of 
    View group -> 
      Components.box 
        [ h1 [] [text "Chart Designer"] 
        , Group.view group (\c -> Card.stub (start c))
        ]
      
    EditingCard group cardState -> 
      Components.box 
        [ h1 [] [text "Chart Designer"] 
        , viewCard cardState start save cancel
        -- , Group.view group.first (\c -> Card.stub c m2)
        ]
    


main = 
  Browser.element 
    { init = init
    , update = update
    , view = (\state -> view state  InitCard SaveCard KillCard )
    , subscriptions = (\_ -> Sub.none)
    }
