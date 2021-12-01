module App exposing (..)

import Mote
import Url.Builder as Url
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


type Model a 
  = Loading
  | Success String
  | ShowMote Mote
  | ParseMote (Result Http.Error Mote)
  | Failure
  | Data a


modLog : Float -> Float -> Float
modLog base val =
  let
    index = logBase base val
  in 
  val / ( base * val)


moteDecoder : Decode.Decoder Mote
moteDecoder =
  Decode.map3 Mote
    (Decode.field "freq" Decode.float)
    (Decode.field "amp" Decode.float)
    (Decode.field "dur" Decode.float)


moteEncoder : Mote -> Encode.Value
moteEncoder ({freq,dur,amp} as mote) =
    Encode.object
        [ ( "freq", Encode.float 100.0 )
        , ( "dur", Encode.float 200.0 )
        , ( "value", Encode.float 200.0 )
        ]

apiUrl : String
apiUrl =
  Url.crossOrigin "http://localhost:3000" [ "acorn" ] []

moteUrl : String
moteUrl =
  Url.crossOrigin "http://localhost:3000" [ "mote" ] []

type alias HTTPData =
  { sending : Bool
  , prevResponse : Bool
  , data : Int
  } 


type alias Module 
  = (Model HTTPData)


type alias Mote =
  { freq : Float
  , amp : Float
  , dur : Float
  }



mote1 : Mote
mote1 =
  { freq = 32.1
  , amp = 0.3
  , dur = 0.5
  }
 
    
type Msg
  = SendRequest
  | SendMoteRequest
  | GotText (Result Http.Error String)
  | GotMote (Result Http.Error Mote)
  | Notify


config : Module
config = Loading


-- decodeMote : Json.Decode.Decoder Mote
-- decodeMote = 
  -- 1


sendHttp : Cmd Msg
sendHttp =
  Http.get 
    { url = apiUrl
    , expect = Http.expectString GotText
    }


body1 =
  mote1


body2 =
  { user = "naltroc"
  , cps = "11"
  , mote = mote1 }


requestMote : Cmd Msg
requestMote =
  let 
    bod = (Http.jsonBody <| moteEncoder mote1)
  in 
  Http.post 
    { url = moteUrl
    , body = bod
    , expect = Http.expectJson GotMote moteDecoder
    }


setup : () -> (Module, Cmd Msg)
setup _ =
  (Loading, sendHttp)


init : Maybe Int -> (Module, Cmd msg)
init flag = 
  (config, Cmd.none)


update : Msg -> Module -> (Module, Cmd Msg)
update msg model =
  case msg of 
    Notify ->
     (model, Cmd.none)

    SendRequest ->
      (Loading, sendHttp)

    SendMoteRequest ->
      (Loading, requestMote)

    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

    GotMote result ->
      case result of
        Ok m ->
          (ShowMote m, Cmd.none)

        Err rrr ->
            (model, Cmd.none)
 

moteItem name value =
  li [ class "list-item" ]
    [ label [ class "label" ] [ text name ]
    , p [] [ text value ] ] 


viewMote : Mote -> Html msg
viewMote {freq, dur, amp} =
  ul [ class "list" ]
    [ moteItem "Frequency" <| String.fromFloat freq
    , moteItem "Duration"  <| String.fromFloat dur
    , moteItem "Volume"  <| String.fromFloat amp
    ] 


postbox : Module -> Html msg
postbox model =
  case model of  
    
    ShowMote m ->
      let 
        root = 32
        cpc = 4
      in 
      div [class "columns"]
        [ Mote.view root cpc  m ]
    
    Success response ->
      text response

    Loading ->
     text "Getting your stuff"

    Failure ->
      text "Doing it wrong ish but doing it butter"

    _ ->
     text "Welcome to the game"


view : Module -> Html Msg
view model =
  div [class "columns"] 
    [ text "hello from module"
    , button [class "button", onClick SendRequest] [ text "request a song" ]
    , button [class "button", onClick SendMoteRequest] [ text "request a mote" ]
    , postbox model

    , h1 [class "title"] [text <| "modLog 2 7:" ++ (String.fromFloat <| modLog 2 2)]
    , br [] []
    ]


subscriptions : Module -> Sub msg
subscriptions model =
  Sub.none

main =
  text ""
