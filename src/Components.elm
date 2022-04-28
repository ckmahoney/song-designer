module Components exposing (..)
-- Generic Html components with Bulma.css selectors

import Types exposing (..)
import Html exposing (Html, text, div)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Data as D
import Tools


type Modal
  = Hidden
  | Showing


type StringTransient a 
  = IsString String
  | IsVal a  


mobileOnlyClass = class "is-hidden-tablet"
mobileNotClass = class "is-hidden-mobile is-hidden-touch"

mobileOnly child =
  div [ mobileOnlyClass ] [ child ]

mobileNot child =
  div [ mobileNotClass ] [ child ]

tabletOnly child =
  div [ class "is-hidden-mobile is-hidden-desktop" ] [ child ]

desktopOnly child =
  div [ class "is-hidden-touch" ] [ child ]

mobile attrs  =
  div (attrs ++ [ class "is-hidden-tablet" ])

tablet  attrs =
  div (attrs ++[ class "is-hidden-desktop is-hidden-mobile" ])

desktop attrs =
  div (attrs ++ [ class "is-hidden-touch" ])





flexRow = class "is-flex is-flex-direction-row"

flexColumn = class "is-flex is-flex-direction-column"


heading : String -> Html msg
heading content =
  Html.h2 [class "title"] [text content]


sectionHeading : String -> Slug -> String -> List (Html msg) -> Html msg
sectionHeading _ slug title buttons =
  div [class "column is-full columns"]
   [ colHalf <| heading title
   , div [class " column is-half has-text-right is-flex is-justify-content-space-between columns is-multiline"] <| 
       List.map col1 (howtoButton title slug :: buttons)
   ]





invertColor : Html msg -> Html msg
invertColor el =
  div [ style "filter" "invert(1)" ] [ el ] 


wrap : Html msg -> Html msg
wrap html =
  div [] [ html ]


wraps : List (Html msg)  -> Html msg
wraps htmls =
  div [] htmls


modal : Modal -> Html msg -> msg -> Html msg
modal state content close =
  case state of
    Hidden -> text ""
    Showing ->
      div [ class "modal is-active" ]
        [ div [ class "modal-background", onClick close ] []
        , div [ class "modal-close is-large", onClick close ] []
        , div [ class "modal-content" ] [ content ]
        ] 


pickerCell : Maybe a -> (a -> Html msg) -> Html msg  -> Html msg
pickerCell el pic html =
  let 
    content = case el of
      Nothing -> text "no item"
      Just x -> pic x 

  in 
  div [ class "box has-background-primary" ] [
    content
    , html
  ] 


keys : Bool -> List (Int, String)
keys useSharps = 
  (if useSharps then D.indexedSharps else D.indexedFlats)


keyMessage : Bool -> Int -> String
keyMessage useSharps root = 
  let  
    (_, tonic) = Tools.getOr root (keys useSharps)  (0, "mysterious")
  in 
  "Key of " ++ tonic


centerText =
 class "has-text-centered"


svg : String -> Html msg
svg name = 
  Html.img [ width 50
      , height 50
      , src <| "/assets/svg/" ++ name ++ ".svg"] []


icon : String -> Html msg
icon name =
  Html.span [ class "icon" ] [ Html.img [ src <| "/assets/svg/" ++ name ++ ".svg"] [] ]

iconWith : String -> String -> Html msg
iconWith classes name =
  Html.span [ class "icon", class classes ] [ Html.img [ src <| "/assets/svg/" ++ name ++ ".svg"] [] ]


svgClick : String -> msg -> Html msg
svgClick name click =
  div [class "is-clickable", onClick click] [svg name]


svgButton : String -> msg -> Html msg
svgButton name click =
  Html.button [onClick click
              , class "is-clickable p-1" ] [svg name]


saveButton : msg -> String -> Html msg
saveButton click cta =
  Html.button [class "button is-size-4 has-background-info has-text-light ", onClick click] [text cta]

type alias Slug = String -- Relative to the absolute url

howtoLink : Slug -> String
howtoLink slug =
  "/" ++ slug

howtoButton : String -> Slug -> Html msg
howtoButton title slug =
  Html.button [class "button is-size-4"] [ Html.a [class "is-flex is-align-items-center is-justify-content-center", href (howtoLink slug), target "_blank"] [text <| "How to: " ++ title] ]

svgButtonClass : String -> String -> msg -> Html msg
svgButtonClass name classes click =
    Html.button [class classes, class "is-clickable p-2", onClick click, style "border-radius" "15%"] [svg name]

svgSquare : String -> Int -> Html msg
svgSquare name x = 
  Html.img [ width x
      , height x
      , src <| "/assets/svg/" ++ name ++ ".svg"] []


deleteIcon : msg -> Html msg
deleteIcon click =
  Html.span [ class "delete", onClick click ] []


deleteButton : msg -> Html msg
deleteButton click =
  Html.button [ class "button is-fullwidth has-background-danger has-text-weight-bold ", onClick click ] [text "Delete"]

closeButton : msg -> Html msg
closeButton click =
  Html.button [ class "delete", onClick click ] []


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

keyPickerFull : Bool -> Int -> (Int -> msg) -> Html msg
keyPickerFull useSharps val toMsg =
  div [class "box"]
    [ div [class "is-hidden-tablet"] [keyPickerMobile useSharps val toMsg]
    , div [class "is-hidden-mobile"] [keyPickerDesktop useSharps val toMsg]
    ]


viewList : List a -> (a -> Html msg) -> Html msg
viewList xs pic =
  box <|
    [ div [ class "columns is-mobile is-multiline" ] <| 
        List.map (\el -> 
          div [ class "column has-text-centered" ]
           [ pic el ]) xs]


buttonDisabled :  (List (Html.Attribute msg)) -> String -> Html msg
buttonDisabled attrs content =
  Html.button ([disabled True, class "button"] ++ attrs) [ text content ]  


strvalToFloat : Float -> Float -> String -> Float
strvalToFloat min max str = 
  let 
    endsDecimal = String.endsWith "." 
    val = Maybe.withDefault min <| String.toFloat str
  in
  if val > max then max else if val < min then min else val


label : String -> Html msg
label content =
  Html.label [class "m-0 mb-3 subtitle"] [ text content ]

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


editRangeString : String -> Html msg -> (Float, Float) -> String -> (String -> msg) -> Html msg
editRangeString title html (mn, mx) val update =
  div [ class "box level" ]
  [ div [class "columns is-multiline"]
    [ div [class "column is-full level is-flex is-justify-content-space-around"] 
      [ Html.label [class "m-0 subtitle"] [text title]
      , Html.input [ type_ "text"
              , onInput update
              , class "m-0"
              , value val ] [] ]
    , div [class "column"] [ html ] ] ]


editInt : String -> Html msg -> (Int,  Int) -> Int -> (Int -> msg) -> Html msg
editInt title html (min, max) val toMsg =
  let 
    num = if val < min then min else if val > max then max else val
    less = if min == num then buttonDisabled [] "-" else
             button (toMsg <| num - 1) [] "-"
    curr = button (toMsg <| num) [] (String.fromInt num)
    more = if max == num then buttonDisabled [] "+" else 
             button (toMsg <| num + 1) [] "+" 
  in 
  div []
    [ div [ class ""]
      [ div [ class "columns is-multiline column is-full"] 
            [ 
            mobile []  
              [  Html.h5 [ class "column is-half subtitle"] [ text title ]
              ,  div  [ class "column is-half is-flex  level"] 
                    [ less
                    , curr
                    , more ]
                ] 
            , tablet [] 
                [ Html.h5 [ class "column  subtitle"] [ text title ]
                , div  [ class "column is-level"] 
                    [ less
                    , curr
                    , more ] 
                ]
            , desktop [ class "columns" ] 
                [ Html.h5 [ class "column is-half subtitle"] [ text title ]
                , div [class "column is-half is-flex level"]
                  [ less
                  , curr
                  , more ] ] ]
      , div [ class "column box has-text-light has-background-info is-full"] [ html ] ] ]


editTextMobile : String -> Html msg -> String -> ( String -> msg ) -> Html msg
editTextMobile title html val toMsg =
  div []
    [ Html.label [class  "is-size-2-touch is-size-2-mobile" ] [ text title ]
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
  div []
    [ mobileOnly <| editTextMobile title html val toMsg 
    , tabletOnly <| editTextDesktop title html val toMsg 
    , desktopOnly <| editTextDesktop title html val toMsg 
    ] 


textEditor : String -> String -> ( String -> msg ) -> Html msg
textEditor p curr update =
  Html.input [ type_ "text"
             , value curr
             , onInput update
             , placeholder p
             , class "input my-3" ] []


editSelection : a -> String -> Html msg -> List (a, (Html msg)) -> a -> (a -> msg) -> Html msg
editSelection curr name info options current select =
  div [ class "box" ]
    [ Html.h5 [ class "subtitle" ] [ Html.label [] [ text name ] ]
    , info
    , colsMulti <|
      List.map (\(val, html) -> 
        let 
          classes = if (val == curr) then "chosen" else ""
        in 
        div [ class "class column is-one-quarter is-flex is-justify-content-center",  onClick (select val) ] [ div [ class "has-text-centered has-background-white", class classes ] [ html ] ]  ) options ]


header : String -> Html msg
header content =
  Html.header [ class "" ] [ text content ]


card : String -> Html msg-> Html msg
card title content = 
  div [ class "card my-3" ] 
    [ Html.header [ class "card-header" ] 
      [ Html.p [ class "card-header-title" ] [ text title ]
      ]
   , div [ class "card-content" ] [ content ]
   ]


cardWith : String -> String -> Html msg-> Html msg
cardWith classes  title content = 
  div [ class "card my-3", class classes ] 
    [ Html.header [ class "card-header has-background-black" ] 
      [ Html.p [ class "has-text-weight-normal card-header-title  has-text-light" ] [ text title ]
      ]
   , div [ class "card-content" ] [ content ]
   ]


card2 : String -> List (Html msg) -> Html msg-> Html msg
card2 title titleMore content = 
  div [ class "card my-3" ] 
    [ Html.header [ class "card-header level" ] 
      [ Html.p [ class "card-header-title" ] [ text title ] 
      , div [ class "mr-4" ] titleMore ] 
      
   , div [ class "card-content" ] 
        [ content ] ]




box : (List (Html msg)) -> Html msg
box =
  div [ class "box" ]


boxWith : String -> (List (Html msg)) -> Html msg
boxWith c =
  div [ class "box", class c ]


boxAttrs attrs  =
  div ([ class "box" ] ++ attrs)


button : msg -> List (Html.Attribute msg) -> String -> Html.Html msg
button click attrs content =
  Html.button ([class "button", onClick click] ++ attrs) [ text content ]


plusButton : msg -> Html msg
plusButton msg  =
  button msg [ class "p-6 is-primary" ] "+"


addButton : msg -> String -> Html msg
addButton msg content =
  button msg [ class "button is-size-4 has-background-success has-text-white" ] content


skButtons : msg -> msg -> Html msg
skButtons saveMsg killMsg  =
  div [class "column is-flex is-justify-content-space-between" ]
    [ button killMsg [] "Delete"
    , button saveMsg [] "Save" ]


editToggle : String -> (a, String) -> (a, String) -> a -> (a -> msg) -> Html msg
editToggle name (x, labelX) (y, labelY) curr toMsg =
  div [ class "box" ]
    [ div [ class "columns is-multiline " ] 
      [ div [ class "columns is-multiline column is-full" ] 
        [ Html.h5 [ class "column is-one-quarter subtitle"] [ text name ]  
        , div [ class "column is-half is-flex is-justify-content-space-between" ] 
          [ button (toMsg x) [class <| if curr == x then "is-success" else ""] labelX
          , button (toMsg y) [class <| if curr == y then "is-success" else ""] labelY
          ] ] ] ]


picker : List a -> (a -> Html msg) -> (Int -> msg) -> Html msg
picker things pic select = 
  div [ class "columns is-multiline level is-vcentered" ] <|
     List.indexedMap (\i thing ->
       div [ class "column is-vcentered has-text-centered", onClick (select i) ]
         [ pic thing ]) things


pickerSelected : List a -> (a -> Html msg) -> (Int -> msg) -> a -> Html msg
pickerSelected things pic select current = 
  div [ class "columns is-multiline level is-vcentered" ] <|
     List.indexedMap (\i thing ->
       div [ class <|if thing == current then "chosen" else "", class "column is-vcentered has-text-centered", onClick (select i) ]
         [ pic thing ]) things

chooserSelected : List a -> (a -> Html msg) -> (a -> msg) -> a -> Html msg
chooserSelected things pic choose current = 
  div [ class "columns is-multiline level is-vcentered" ] <|
     List.map (\thing ->
       div [ class <|if thing == current then "chosen" else "", class "column is-vcentered has-text-centered", onClick (choose thing) ]
         [ pic thing ]) things


killer : List a -> (a -> Html msg) -> (Int -> msg) -> Html msg
killer things pic kill = 
  div [ class "columns is-multiline level is-vcentered" ] <|
     (List.indexedMap (\i thing ->
       div [ class "column is-flex is-flex-direction-column is-align-items-center is-justify-content-center" ]
         [ col [ class "is-full has-text-centered" ] [(pic thing)]
         , col [ class "is-full has-text-centered" ] [(deleteIcon (kill i))] ] ) things)


pickerAnd : List a -> (Html  msg) -> (a -> Html msg) -> (Int -> msg) ->  Html msg
pickerAnd things more pic select = 
  div [ class "columns is-multiline level is-vcentered" ] <|
     (List.indexedMap (\i thing ->
       div [ class "column is-vcentered has-text-centered", onClick (select i) ]
         [ pic thing ] ) things) ++ [more]


pickerKiller : List a -> (a -> Html msg) -> (Int -> msg) ->  (Int -> msg) -> Html msg
pickerKiller things pic select kill = 
  div [ class "columns is-multiline level is-vcentered" ] <|
     (List.indexedMap (\i thing ->
       div [ class "column is-flex is-flex-direction-column is-align-items-center is-justify-content-center" ]
         [ col [ class "is-full has-text-centered", onClick (select i) ] [(pic thing)]
         , col [ class "is-full has-text-centered" ] [(deleteIcon (kill i))] ] ) things)


pickerKillerAnother : List a -> (Html  msg) -> (a -> Html msg) -> (Int -> msg) ->  (Int -> msg) -> Html msg
pickerKillerAnother things more pic select kill = 
  div [ class "columns is-multiline level is-vcentered" ] <|
     (List.indexedMap (\i thing ->
       div [ class "column is-flex is-flex-direction-column is-align-items-center is-justify-content-center" ]
         [ col [ class "is-full has-text-centered", onClick (select i) ] [(pic thing)]
         , col [ class "is-full has-text-centered" ] [(deleteIcon (kill i))] ] ) things) ++ [more]


colsWith : List (Html.Attribute msg) -> (List (Html msg) -> Html msg)
colsWith attrs =
  div ([ class "columns" ] ++ attrs) 


cols :  (List (Html msg) -> Html msg)
cols  =
  div [ class "columns" ]


colsFor : (List (Html msg)) -> Html msg
colsFor children =
  cols <| List.map (\c -> div [class "column"] [c]) children


colsMulti :  (List (Html msg) -> Html msg)
colsMulti  =
  div [ class "columns is-multiline" ]


col : List (Html.Attribute msg) -> (List (Html msg) -> Html msg)
col attrs =
  div ([ class "column" ] ++ attrs) 

col1 : Html msg -> Html msg
col1 child =
  div [ class "column" ] [child]


colSize : String -> Html msg -> Html msg
colSize size child =
  div [ class "column", class size ] [ child ]


colHalf : Html msg -> Html msg
colHalf child =
  colSize "is-half" child

colFull : Html msg -> Html msg
colFull child =
  colSize "is-full" child


songCard : String -> List (Html msg) -> Html msg
songCard title icons =
  div []
    ([ text title ] ++ icons)


paragraph : String -> Html msg
paragraph c =
  Html.p [ class "content" ] [ text c ] 
  

main = 
  Html.text ""

