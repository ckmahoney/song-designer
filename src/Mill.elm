module Mill exposing (Msg, Model, init, view, update, subscriptions, main, initWith)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Msg a
  = Newing 
  | Adding a
  | Killing a
  | Cloning a
  -- | Choosing next
  -- | Viewing curr


-- type alias Model a =
  -- { current : Maybe a
  -- , list : List a
  -- }


type alias Body msg =
  { body : Html msg }


type alias Shell a =
  { current : Maybe a
  , list : List a
  }


type alias Model a = (Shell  a)


-- initModel : Model a


conj x xs =
  List.append xs [x]


remove : Maybe a -> List a -> List a
remove x xs =
  case x of 
   Nothing ->
     xs

   Just jx ->
     List.filter (\a -> not (jx == a)) xs



emptyHolder : Shell a
emptyHolder = 
  { current = Nothing
  , list = []
  }  


init : Shell a
init =
  emptyHolder


initWith : a -> Shell a
initWith x =
  { current =  Just x
  , list = List.singleton x
  }


galleryItem : a -> Html (Msg a)
galleryItem el =
  case el of 
    -- x ->
       -- div [ class "box column" ] [ text x ]

    _ -> 
       div [ class "box column" ] [ text  "hahaha yeah bud" ]
   


gallery : List a -> Html (Msg a)
gallery els =
  div [ class "box columns" ]
    ([ h2 [ class "subtitle" ] [ text "All" ] 
    ]
    ++ (List.map (\e -> galleryItem e) els))


editor : a -> Html (Msg a)
editor model =
  div [ class "editor" ] 
    [ h2 [ class "subtitle" ] [ text "Edit" ]
    ]


initMessage : List a ->  Html (Msg a)
initMessage list = 
  div []
  [ p [ class "content" ] [ text "Click here to create a new one from scratch." ] 
  , text (if 0 == List.length list then "" else "Or, update an existing from below.") ]


updateModel : Msg a -> Shell a -> Shell a
updateModel msg model =
  case msg of 
    Newing ->
      init

    Adding next ->
     case model.current of 
      Nothing ->
        { model 
        | current = Just next }
      
      Just prev ->
        { model 
        | current = Just next 
        , list = conj prev model.list }

    Killing prev ->
      { model | list = remove (Just prev) model.list }

    Cloning prev ->
      { model | list = conj prev model.list }


update : Msg a -> Model a -> (Model a, Cmd (Msg a))
update msg model =
  ((updateModel msg model), Cmd.none)


view : Shell a -> Html (Msg a)
view {current,list} =
  case current of
    Nothing ->
      div [] [ initMessage list ]
  
    Just curr ->
      div [] 
      [ editor curr 
      , gallery list
      , button [onClick (Adding curr)] [text "more"]
      , button [onClick (Cloning curr)] [text "clone"]
      , button [onClick (Killing curr)] [text "less"]
      ]


subscriptions model =
  Sub.none


main = text ""
