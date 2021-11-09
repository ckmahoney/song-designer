module Update exposing (..)

import Model exposing (Model)
import Html

type Msg
    = ButtonClick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ButtonClick ->
            ({ model
                | counter = model.counter + 1
            }, Cmd.none)
                

main =
  Html.text ""

