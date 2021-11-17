module Components exposing (..)


import Html exposing (Html, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


button : msg -> List (Html.Attribute msg) -> String -> Html.Html msg
button toMsg attrs content =
  Html.button ([class "button", onClick toMsg] ++ attrs) [ text content ]


skButtons : msg -> msg -> Html msg
skButtons saveMsg killMsg  =
  Html.div [class "column is-flex is-justify-content-space-between" ]
    [ button killMsg [] "Delete"
    , button saveMsg [] "Save" ]


picker : List (Html msg) -> msg -> Html msg
picker options select =
  Html.div [] 
    <| List.map (\opt -> Html.div [ onClick select ] [ opt ]) options


-- everything you need to create, read, update, and delete a thing
crud : Html msg -> ( x -> Html msg ) -> x -> msg -> msg -> msg -> msg -> Html msg
crud viewer editor thing add kill select patch =
  Html.div [] [ viewer, editor thing ]


edit : a -> (a -> Html a) -> msg -> Html msg
edit thing view patch =
  Html.div [] []


designer : Html msg -> (a -> Html msg) -> a -> Html msg
designer view editor thing = 
  Html.div [] 
    [ view, editor thing ]


type alias Model 
  = ()


type Msg
  = Input Int


init : flags -> ( Model, Cmd Msg )
init _ =
  ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of 
    Input n ->
      ( model, Cmd.none )


main = 
  Html.text ""

