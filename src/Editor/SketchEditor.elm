module Editor.SketchEditor exposing (view, main)

import Browser
import Html exposing (Html, button, div, text, label, p, input,b)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onInput)

import Defs exposing (Sketch, Duty(..), Relation(..))
import View 
import Elements
import Tools
import Components

type alias Original = Sketch
type alias Updating = Sketch

type alias Model = (Original, Maybe Updating)

type Msg = 
  Pass


sizes : List Int
sizes = 
  List.range 0 7


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)


initModel : Model
initModel = 
  (Sketch "New Sketch" Declare Source 2, Nothing)

init : (Maybe Int) -> (Model, Cmd msg)
init flags =
  (initModel, Cmd.none)

-- Shows the high level details of a Sketch.
shell : Sketch -> Html msg
shell sketch = 
  div []
    [ Components.label sketch.label
    , Components.label <| "Duty : " ++ Defs.dutyLabel sketch.duty
    , Components.label <| "Relation : " ++ Defs.relationLabel sketch.relation
    , Components.label <| "Size : " ++ String.fromInt sketch.size
    ]


view : Model -> Html msg
view model = 
  case model of 
    (ref, Nothing) -> shell ref
    _ -> text "Need a case for the rest"


main = Browser.element 
  { init = init
  , update = update
  , view = view
  , subscriptions = (\_ -> Sub.none)
  }
