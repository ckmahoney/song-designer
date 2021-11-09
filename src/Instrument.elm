module Instrument exposing (main)

import Browser
import Dict
import Html exposing (Html, h1, button, div, text, label, p, input, select, option)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL



type Role
  = Kick
  | Perc
  | Hat
  | Bass
  | Chords
  | Melody

type alias RRole
  = (Role, String)

roles : List RRole
roles = 
  [ (Kick, "Kick Drum")
  , (Perc, "Percussion")
  , (Hat, "Hats")
  , (Bass, "Bass")
  , (Chords, "Chords")
  , (Melody, "Melody")
  ]

type alias Instrument =
  { voice : Int
  , role : (Role, String)
  , density : Int
  , complexity : Int
  , title : String
  }

type alias Model = Instrument


init : Model
init =
  { voice = 1
  , role = (Melody, "Melody")
  , density = 0
  , complexity = 0
  , title = "Synthy"
  }


toInt : Maybe Int ->  Int
toInt x =
    case x of 
      Nothing ->
         0
      Just i -> 
        i


-- UPDATE

type Field
  = Voice
  | Density 
  | Complexity

type ParamChange
  = Numeric Field Int
  | Title String
  | Assignment (Role, String)

type Msg
  =
   Update ParamChange


recChange : ParamChange -> Model -> Model
recChange sig model =
  case sig of
    Numeric field val ->
      case field of 
        Voice ->
         { model | voice = val }
        Density ->
          { model | density = val }
        Complexity ->
          { model | complexity = val }
    Title val ->
      { model | title = val }

    Assignment (r,n) ->
      { model | role = (r,n) }


update : Msg -> Model -> Model
update msg model =
  case msg of

    Update param ->
      recChange param model
      

-- VIEW



recField : String -> Field -> Int -> Html Msg 
recField name field val = 
  div []
    [ label [] [ text name ]
    , p [] [text (String.fromInt val)]
    , button [ onClick (Update (Numeric field (val + 1))) ] [ "More " ++ name |> text ]
    , button [ onClick (Update (Numeric field (val - 1))) ] [ "Less " ++ name |> text ]
    ]


titleField : String -> Html Msg
titleField title =
  div []
    [ label [] [text "Synth Name"]
    , input [ placeholder "Melody", value title, onInput (\s -> (Update (Title s))) ] []
    ] 


getRole : String -> Role
getRole name =
  let 
    el =  (List.filter (\(role,n) -> 
      if n == name then 
        True 
      else
        False
     ) roles) |> List.head
  in 
  (Tuple.first (Maybe.withDefault (Bass, "") el))


roleOption : RRole -> RRole -> Html Msg
roleOption selected (role, label) =
  option [value label] [text label]


roleField : RRole -> Html Msg
roleField selected = 
  div [] 
    [ label [] ["Using role " ++  (Tuple.second selected) |> text]
    , select [onInput (\s ->  (Update (Assignment ((getRole s), s))))] (List.map (roleOption selected) roles)
    ] 


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [text "Synth Designer"]
    , titleField model.title
    , roleField model.role
    , recField "Voice" Voice (.voice model)  
    , recField "Density" Density (.density model)  
    , recField "Complexity" Complexity (.complexity model)  
    ]
