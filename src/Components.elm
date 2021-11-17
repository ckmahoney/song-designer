module Components exposing (..)


import Html exposing (Html, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


noClickButton : Html msg
noClickButton =
  Html.button [ class "m-0 button image is-48x48 has-background-black"
         , style "border-radius" "50%"
         , style "cursor" "initial"] []


editInt : String -> Html msg -> (Int,  Int) -> Int -> (Int -> msg) -> Html msg
editInt title html (min, max) val toMsg =
  let 
    less = if min == val then noClickButton else
             button (toMsg <| val - 1) [class "image button is-48x48"] "-"
    more = if max == val then noClickButton else 
             button (toMsg <| val + 1) [class "image button is-48x48" ] "+ "
  in 
  Html.div [ class "m-3 box"]
    [ Html.div [ class "columns is-multiline"]
      [ Html.div [ class "columns is-multiline column is-full is-flex is-flex-row is-justify-content-space-between"] 
            [ Html.h5 [ class "column is-full subtitle"] [ text title ]
            , Html.div [ class "mt-0 column is-full is-flex is-flex-row level"] 
                     [ less
                     , Html.b [] [" " ++ String.fromInt val |> text ]
                     , more ] ] 
      , Html.div [ class "column box has-text-light has-background-info is-full"] [ html ] ] ]


editTextMobile : String -> Html msg -> String -> ( String -> msg ) -> Html msg
editTextMobile title html val toMsg =
  Html.div []
    [ Html.label [ ] [ text title ]
    ,  Html.input [ type_ "text"
            , class "input my-3 is-info"
            , value val
            , onInput toMsg ] []
    , html
    ]


editTextDesktop : String -> Html msg -> String -> ( String -> msg ) -> Html msg
editTextDesktop title html val toMsg =
  Html.div []
    [ Html.label [ ] [ text title ]
    , Html.input [ type_ "text"
            , class "input my-3 is-info"
            , value val
            , onInput toMsg ] []
    , html
    ]


editText : String -> Html msg -> String -> ( String -> msg ) -> Html msg
editText title html val toMsg =
  Html.div [class "m-3 box"]
    [ Html.div [ class "is-hidden-tablet" ] [ editTextMobile title html val toMsg ]
    , Html.div [ class "is-hidden-mobile" ] [ editTextDesktop title html val toMsg ] ] 


button : msg -> List (Html.Attribute msg) -> String -> Html.Html msg
button toMsg attrs content =
  Html.button ([class "button", onClick toMsg] ++ attrs) [ text content ]


skButtons : msg -> msg -> Html msg
skButtons saveMsg killMsg  =
  Html.div [class "column is-flex is-justify-content-space-between" ]
    [ button killMsg [] "Delete"
    , button saveMsg [] "Save" ]


picker : List (Html msg) -> (Int -> msg) -> Html msg
picker options select =
  Html.div [] 
    <| List.indexedMap (\index opt -> Html.div [ onClick (select index) ] [ opt ]) options


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


editOneOfMany : List (Html msg) -> Maybe a -> (a -> Html msg) -> Html msg
editOneOfMany xs x editor =
  let
    v = case x of
          Nothing -> text "" 
          Just xx -> editor xx
  in 
  Html.div [] 
    ([ v ] ++ xs)


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

