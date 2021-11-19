module Components exposing (..)


import Html exposing (Html, text, div)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Data as D


invertColor : Html msg -> Html msg
invertColor el =
  div [ style "filter" "invert(1)" ] [ el ] 


wrap : Html msg -> Html msg
wrap html =
  div [] [ html ]


wraps : List (Html msg)  -> Html msg
wraps htmls =
  div [] htmls




svg : String -> Html msg
svg name = 
  Html.img [ width 50
      , height 50
      , src <| "/svg/" ++ name ++ ".svg"] []


deleteIcon : msg -> Html msg
deleteIcon click =
  Html.span [ class "delete", onClick click ] []


keyButtonMobile : String -> Int -> (Int -> msg) -> Int -> Html msg
keyButtonMobile name curr toMsg val =
  Html.button [ onClick (toMsg val)
         , class <| if curr == val then "is-success is-selected" else ""
         , class "column p-0 is-4 button" ] [text name]


keyButtonDesktop : String -> Int -> (Int -> msg) -> Int -> Html msg
keyButtonDesktop name curr toMsg val =
  Html.button [ onClick (toMsg val)
         , class <| if curr == val then "is-success is-selected" else ""
         , class "column is-3 button p-0" ] [text name]


keyPickerMobile : Bool -> Int -> (Int -> msg) -> Html msg
keyPickerMobile useSharps val toMsg =
  div [class "key-picker-mobile"] 
    <| [ Html.h5 [class "subtitle"] [text "Key"] ]
    ++ [ div [class "columns is-multiline is-mobile"] 
       <| List.map (\(v, name) -> keyButtonDesktop name val toMsg v) (if useSharps then D.indexedSharps else D.indexedFlats) ]


keyPickerDesktop : Bool -> Int -> (Int -> msg) -> Html msg
keyPickerDesktop useSharps val toMsg =
  div [class ""] 
    <| [ Html.h5 [class "subtitle"] [text "Key"] ] 
    ++ [ div [class "columns is-multiline"] 
       <| List.map (\(v, name) -> keyButtonDesktop name val toMsg v) (if useSharps then D.indexedSharps else D.indexedFlats) ]


keyPicker : Bool -> Int -> (Int -> msg) -> Html msg
keyPicker useSharps val toMsg =
  div [class "box container"]
    [ div [class "is-hidden-tablet"] [keyPickerMobile useSharps val toMsg]
    , div [class "is-hidden-mobile"] [keyPickerDesktop useSharps val toMsg]
    ]



noClickButton : Html msg
noClickButton =
  svg "empty"


strvalToFloat : Float -> Float ->  String -> Float
strvalToFloat min max str = 
  let 
    val = Maybe.withDefault min <| String.toFloat str
  in
  if val > max then max else if val < min then min else val


editRange : String -> Html msg -> (Float, Float) -> Float -> (Float -> msg) -> Html msg
editRange title html (mn, mx)  val fltMsg =
  div [ class "box level"
           , onInput (\str -> fltMsg (strvalToFloat mn mx str)) ]
  [ div [class "columns is-multiline"]
    [ div [class "column is-full level is-flex is-justify-content-space-around"] 
      [ Html.label [class "m-0 subtitle"] [text title]
      , Html.input [ type_ "text"
              , class "m-0"
              , value <| String.fromFloat val ] [] ]
    , div [class "column"] [ html ] ] ]


editInt : String -> Html msg -> (Int,  Int) -> Int -> (Int -> msg) -> Html msg
editInt title html (min, max) val toMsg =
  let 
    less = if min == val then noClickButton else
             button (toMsg <| val - 1) [class "image button is-48x48"] "-"
    more = if max == val then noClickButton else 
             button (toMsg <| val + 1) [class "image button is-48x48" ] "+ "
  in 
  div [ class "box"]
    [ div [ class "columns is-multiline"]
      [ div [ class "columns is-multiline column is-full"] 
            [ Html.h5 [ class "column is-one-quarter subtitle"] [ text title ]
            , div [ class "column is-half is-flex is-flex-row level"] 
                     [ less
                     , Html.b [] [" " ++ String.fromInt val |> text ]
                     , more ] ] 
      , div [ class "column box has-text-light has-background-info is-full"] [ html ] ] ]


editTextMobile : String -> Html msg -> String -> ( String -> msg ) -> Html msg
editTextMobile title html val toMsg =
  div []
    [ Html.label [ ] [ text title ]
    ,  Html.input [ type_ "text"
            , class "input my-3 is-info"
            , value val
            , onInput toMsg ] []
    , html
    ]


editTextDesktop : String -> Html msg -> String -> ( String -> msg ) -> Html msg
editTextDesktop title html val toMsg =
  div []
    [ Html.h5 [ class "subtitle" ] [ Html.label [] [ text title ] ]
    , Html.input [ type_ "text"
            , class "input my-3 is-info"
            , value val
            , onInput toMsg ] []
    , html
    ]


editText : String -> Html msg -> String -> ( String -> msg ) -> Html msg
editText title html val toMsg =
  div [class "box"]
    [ div [ class "is-hidden-tablet" ] [ editTextMobile title html val toMsg ]
    , div [ class "is-hidden-mobile" ] [ editTextDesktop title html val toMsg ] ] 


editSelection : a -> String -> Html msg -> List (a, (Html msg)) -> a -> (a -> msg) -> Html msg
editSelection curr label info options current select =
  div [ class " box" ]
    [ Html.h5 [ class "subtitle" ] [ Html.label [] [ text label ] ]
    , info
    , div [ class "columns is-multiline" ] <|
      List.map (\(val, html) -> 
        let 
          fx = if (val == curr) then invertColor  else wrap
        in 
        fx <| div [ class "has-background-white column", onClick (select val) ] [ html ]) options ]


card : String -> Html msg-> Html msg
card title content = 
  div [ class "card my-3" ] 
    [ Html.header [ class "card-header" ] 
      [ Html.p [ class "card-header-title" ] [ text title ]
      ]
   , div [ class "card-content" ] 
        [ content ] ] 


box =
  div [ class "box" ]



button : msg -> List (Html.Attribute msg) -> String -> Html.Html msg
button toMsg attrs content =
  Html.button ([class "button", onClick toMsg] ++ attrs) [ text content ]


skButtons : msg -> msg -> Html msg
skButtons saveMsg killMsg  =
  div [class "column is-flex is-justify-content-space-between" ]
    [ button killMsg [] "Delete"
    , button saveMsg [] "Save" ]


modal : Html msg -> msg -> Html msg
modal content close =
  div [ class "modal is-active" ]
    [ div [ class "modal-background", onClick close ] []
    , div [ class "modal-close is-large", onClick close ] []
    , div [ class "modal-content" ] [ content ]
    ] 


editToggle : String -> (a, String) -> (a, String) -> a -> (a -> msg) -> Html msg
editToggle label (x, labelX) (y, labelY) curr toMsg =
  div [ class "box" ]
    [ div [ class "columns is-multiline " ] 
      [ div [ class "columns is-multiline column is-full" ] 
        [ Html.h5 [ class "column is-one-quarter subtitle"] [ text label ]  
        , div [ class "column is-half is-flex is-justify-content-space-between" ] 
          [ button (toMsg x) [class <| if curr == x then "is-success" else ""] labelX
          , button (toMsg y) [class <| if curr == y then "is-success" else ""] labelY
          ] ] ] ]


picker : List a -> (a -> Html msg) -> (Int -> msg) -> Html msg
picker things icon select = 
  div [ class "columns is-multiline level is-vcentered" ] <|
     List.indexedMap (\i thing ->
       div [ class "column is-vcentered has-text-centered", onClick (select i) ]
         [ icon thing ]) things


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

