module ColorPicker exposing (Model, Msg(..), colorSlider, initialModel, main, update, view)

import Browser
import Color exposing (Color)
import Html
import Html.Attributes as Attr
import Html.Events as Evt


type alias Model =
    { color : Color
    }


initialModel : Model
initialModel =
    { color = Color.rgb 50 200 100 }


type Msg
    = UpdateColor Color


update : Msg -> Model -> Model
update msg model =
    let
        { red, green, blue } =
            Color.toRgb model.color
    in
    case msg of
        UpdateColor newColor ->
            { model | color = newColor }


colorSlider : String -> Int -> (Int -> msg) -> Html.Html msg
colorSlider name colorValue toMsg =
    Html.div []
        [ Html.p [] [ Html.text name ]
        , Html.input
            [ Attr.type_ "range"
            , Attr.name ("color" ++ name)
            , Attr.min "0"
            , Attr.max "255"
            , Attr.value (String.fromInt colorValue)
            , Evt.onInput (toMsg << toInt colorValue)
            ]
            []
        , Html.span [] [ Html.text (String.fromInt colorValue) ]
        ]


colorPicker : Color -> (Color -> msg) -> Html.Html msg
colorPicker color toMsg =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    Html.div []
        [ colorSlider "Red" red (toMsg << redToColour color)
        , colorSlider "Green" green (toMsg << greenToColour color)
        , colorSlider "Blue" blue (toMsg << blueToColour color)
        , Html.div
            [ Attr.style "width" "100px"
            , Attr.style "height" "100px"
            , Attr.style "background-color" (toColorCss color)
            ]
            []
        ]


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ colorPicker model.color UpdateColor
        ]


toInt : Int -> String -> Int
toInt defaultValue strValue =
    strValue
        |> String.toInt
        |> Maybe.withDefault defaultValue


redToColour : Color -> Int -> Color
redToColour color newRed =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    Color.rgb newRed green blue


greenToColour : Color -> Int -> Color
greenToColour color newGreen =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    Color.rgb red newGreen blue


blueToColour : Color -> Int -> Color
blueToColour color newBlue =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    Color.rgb red green newBlue


toColorCss : Color -> String
toColorCss color =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    "rgb("
        ++ String.fromInt red
        ++ ","
        ++ String.fromInt green
        ++ ","
        ++ String.fromInt blue
        ++ ")"


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
