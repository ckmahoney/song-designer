module LayoutEditor exposing (..)

import Browser
import Html exposing (Html, button, div, text, label, p, input,b)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onInput)


import Types exposing (..)
import View 
import Elements
import Tools
import Components
import Data
import ComboEditor
import ScopeEditor
import EnsembleEditor
import VoiceEditor

import Json.Decode as Decode


type alias State = List Combo


type Msg
  = Update Int Combo
  | Save State
  | Local ComboEditor.Model Internal
  | Cloning Int 


type Internal 
  = Create
  | Select Int
  | Kill Int
  | Edit Int Combo


type Model 
  = Overview State
  | Editing State Int ComboEditor.Model
  | PlacingClone State Combo

type alias DragItem = String


type alias DragModel =
    { beingDragged : Maybe DragItem
    , draggableItems : List DragItem
    , items : List DragItem
    }


initialModel : DragModel
initialModel =
    { beingDragged = Nothing
    , draggableItems =
        List.range 1 5
            |> List.map Debug.toString
    , items = []
    }


type DragMsg
    = Drag DragItem
    | DragEnd
    | DragOver
    | Drop


updateDrag : DragMsg -> DragModel -> DragModel
updateDrag msg model =
    case msg of
        Drag item ->
            { model | beingDragged = Just item }

        DragEnd ->
            { model | beingDragged = Nothing }

        DragOver ->
            model

        Drop ->
            case model.beingDragged of
                Nothing ->
                    model

                Just item ->
                    { model
                        | beingDragged = Nothing
                        , items = item :: model.items
                    }


draggableItemView : String -> Html DragMsg
draggableItemView item =
    Html.div
        [ Attr.class "card fluid warning"
        , Attr.draggable "true"
        , onDragStart <| Drag item
        , onDragEnd DragEnd
        ]
        [ Html.div
            [ Attr.class "section" ]
            [ Html.text item ]
        ]


itemView : String -> Html DragMsg
itemView item =
    Html.div
        [ Attr.class "card fluid error" ]
        [ Html.div
            [ Attr.class "section" ]
            [ Html.text item ]
        ]


viewDragger : DragModel -> Html DragMsg
viewDragger model =
    Html.div
        [ Attr.class "container" ]
        [ Html.div
            [ Attr.class "row" ]
            [ Html.div
                [ Attr.class "col-sm-6" ]
              <|
                (List.map draggableItemView model.draggableItems
                    |> (::) (Html.h4 [] [ Html.text "Draggable" ])
                )
            , Html.div
                [ Attr.class "col-sm-6"
                , onDragOver DragOver
                , onDrop Drop
                ]
              <|
                (List.map itemView model.items
                    |> (::) (Html.h4 [] [ Html.text "Drop Zone" ])
                )
            ]
        ]


onDragStart msg =
    Events.on "dragstart" <|
        Decode.succeed msg


onDragEnd msg =
    Events.on "dragend" <|
        Decode.succeed msg


onDragOver msg =
    Events.preventDefaultOn "dragover" <|
        Decode.succeed ( msg, True )


onDrop msg =
    Events.preventDefaultOn "drop" <|
        Decode.succeed ( msg, True )



apply : State -> Internal -> State
apply state msg =
  case msg of 
   Select index ->
      state 

   Create -> 
    let
      v = Data.emptyCombo
      next = List.append state [ v ]
      index = List.length next
    in 
    next 

   Kill index ->
    Tools.removeAt index state


   Edit index combo ->
     Tools.replaceAt index combo state


curr state index =
  Tools.getOr index state Data.emptyCombo


edit : State -> Internal -> ComboEditor.Model -> Model
edit state msg mod =
  case msg of 
   Select index ->
     Editing state index <| ComboEditor.Overview (curr state index)

   Create -> 
    let
      next = apply state msg
    in
    Overview  next

   Kill index ->
    Overview <| Tools.removeAt index state

   Edit index combo ->
     Editing state index  mod


update : Msg -> State -> Model
update msg state =
  case msg of 
   Save next->
    Overview next

   Update index combo ->
     let
      next = Tools.replaceAt index combo state
     in
      Overview next

   Local mod internal ->
     edit state internal mod

   Cloning index ->
    PlacingClone state (curr state index)

initState : State
initState = 
 let
  ((meta, layout) as template) = Data.templateVerseChorus
 in
  layout
  -- []


initTest = 
  Overview initState
  -- Editing initState 0 <| ComboEditor.initModel initState 0 

initModel =
  Just <| Overview initState


init =
  Overview


lookOld things icon =
  Components.box
   [ Html.h2 [ Attr.class "subtitle" ] [text "Layout"]
   , div [ Attr.class "columns is-multiline level is-vcentered" ]
       [ p [ Attr.class "content"] 
           [ text "Here is your layout."
           , Html.br [] []
           , Html.br [] []
           , text "Each combo has a name, some voices, and duration in seconds."] ]
   , div [ Attr.class "columns is-multiline level is-vcentered is-flex is-justify-content-flex-start" ] <|
     (List.indexedMap (\i thing ->
       div [ Attr.class "is-flex is-flex-direction-column" ]
         [ Components.col [ Attr.class "is-full has-text-centered" ] [(icon thing)]
         ] ) things)

   ]


controls select kill clone = 
  div [Attr.class "column columns is-flex-direction-column picker-icons is-one-quarter "]
    [ Components.col [ Attr.class "has-text-centered is-clickable " ] [Components.svgButtonClass "settings" "has-background-primary" select]
    , Components.col [ Attr.class "has-text-centered" ] [Components.svgButtonClass "clone" "has-background-info" clone]
    , Components.col [ Attr.class "has-text-centered is-clickable " ] [Components.svgButtonClass "trash" "has-background-danger" kill] 
    ]


comboCard icon combo i select kill clone =
  div [ Attr.class "is-mobile columns is-flex my-3", Attr.style "border" "1px solid lightgrey",  Attr.style "border-radius" "5px" ]  [ Components.colSize "is-three-quarters has-text-centered" <| Components.col1 <| icon combo
     , controls select kill clone
     ]
 

picker things icon select kill clone add = 
  Components.box
   [ Html.h2 [ Attr.class "subtitle" ] [text "Layout"]
   , div [ Attr.class "columns is-multiline level is-vcentered  is-flex-direction-column" ] <|
    List.append 
     (List.indexedMap (\i thing ->
       (comboCard icon thing i (select i) (kill i) (clone i))) things)
  
      [ if 4 > List.length things then 
          Components.col1 <| Components.plusButton add
        else text ""
      ]
   ]

pickerHorizontal things icon select kill clone add = 
  Components.box
   [ Html.h2 [ Attr.class "title" ] [text "Layout"]
   , div [ Attr.class "columns is-multiline" ] <|
    List.append 
     (List.indexedMap (\i thing ->
       div [Attr.class "column is-flex is-flex-directtoin-row is-mobile is-half is-level"] 
         [ comboCard icon thing i (select i) (kill i) (clone i)
         ]) things)
      [ if 4 > List.length things then 
          Components.col1 <| Components.plusButton add
        else text ""
      ]
   ]


placerDesktop things icon place clone =
  div [ Attr.class "inserting columns is-multiline level is-vcentered  is-flex-direction-column" ] <|
     (List.indexedMap (\i thing ->
       div [ Attr.class "is-clickable columns column is-flex is-flex-direction-column is-half my-3"
           , onClick (place i) ]
             [ div [Attr.class "uncover"] [ Components.plusButton (place i) ]
             , icon thing
             ] ) things)

placerMobile things icon place clone =
  div [ Attr.class "columns is-multiline level is-vcentered  is-flex-direction-column" ] <|
     (List.indexedMap (\i thing ->
       div [ Attr.class "is-clickable columns column is-flex is-flex-direction-column is-half my-3", onClick (place i) ]
         [ Components.colFull <| div [ Attr.class "is-full has-text-centered"] 
           [ div [onClick (place i), Attr.class "py-6"] [ Components.plusButton (place i) ]
           , icon thing
           ]
         ] ) things)

placer things icon place clone = 
  Components.box
   [ Html.h2 [ Attr.class "subtitle" ] [text "Layout - Cloning a Combo"]
   , div [Attr.class "mb-3" ] 
       [ b [] [text "Cloning this combo:"]
       , div [ Attr.class "has-background-warning"] <| List.singleton <|
           Components.colSize "is-half is-offset-one-quarter" <| (icon clone)
       , b [] [text "Choose a place to put it."]
       ]
   , Components.mobile []  [ placerMobile things icon place clone ]
   , Components.tablet []  [ placerMobile things icon place clone ]
   , Components.desktop [] [ placerDesktop things icon place clone ]
   ]


comboIcon : Combo -> Html msg
comboIcon ((scope, ensemble) as model) =
  div [ Attr.class "box" ] 
    [ label [ Attr.class "label" ] [ text <| scope.label ]
    , Components.svg "ensemble"
    , p [ Attr.class "content" ] [ text <| (String.fromInt <| List.length ensemble) ++ " voices" ]
    ]


look : State -> List (Html msg)
look layout =
  List.map ComboEditor.thumb layout


create : State -> State
create state  =
 List.append state [ Data.emptyCombo ]
 


createMsg : State -> Msg
createMsg state = 
  let 
   next = create state
   i = List.length next
   v = Tools.getOr
  in 
  Save next


fromCombo : State -> Int -> Combo -> State
fromCombo state index combo =
  Tools.replaceAt index combo state





displayMobile state model forward save close  =
  let 
   kill = (\i -> forward <| update (Save <| Tools.removeAt i state) state)
   select = (\i -> (forward <| edit state (Select i) <| ComboEditor.initModel state i))
   clone = (\i -> forward <| update (Cloning i) state)
   add = (forward <| update (Save (apply state Create)) state)
  in 
   Components.box <|
     [ div [ Attr.class "columns is-multiline level is-vcentered" ]
       [ p [ Attr.class "content"] 
           [ text "Organize the parts of your sound. "    ] ]
     , picker state View.viewComboVertical select kill clone add
     , Components.button (close state) [] "Close" 
     ]


displayDesktop state model forward save close  =
  let 
   kill = (\i -> forward <| update (Save <| Tools.removeAt i state) state)
   select = (\i -> (forward <| edit state (Select i) <| ComboEditor.initModel state i))
   clone = (\i -> forward <| update (Cloning i) state)
   add = (forward <| update (Save (apply state Create)) state)
  in 
   Components.box <|
     [ div [ Attr.class "columns is-multiline level is-vcentered" ]
       [ p [ Attr.class "content"] 
           [ text "Organize the parts of your sound. "    ] ]

     , pickerHorizontal state View.viewComboVertical select kill clone add
     , Components.button (close state) [] "Close" 
     ]



view : Model -> (Model -> msg) -> (State -> msg) -> (State -> msg) -> Html msg
view model forward save close  =
  case model of 
    Overview state ->
      div [] 
        [ Components.mobile [] <| List.singleton <| displayMobile state model forward save close
        , Components.desktop [] <|List.singleton <|  displayDesktop state model forward save close
        ]

    PlacingClone state combo  ->
     let 
      place = (\i -> (forward <| update (Save <| Tools.insertAt i combo state) state))
     in 
      Components.box <|
        [ div [ Attr.class "columns is-multiline level is-vcentered" ]
          [ p [ Attr.class "content"] 
              [ text "You are making a clone of one of your combos."    
              , Html.br [] []
              , Html.br [] []
              , text "Select the combo to put it before." ] 
              , Html.br [] [] ]
        , placer state View.viewCombo place combo
        , Components.button (close state) [] "Close" 
        ]

    Editing state index mod ->
      let 
        continue = forward << edit state (Edit index (curr state index))
        swap = (\c ->  Tools.replaceAt index c state)
        keep =  (\combo -> forward <| (update (Save <| swap combo) state))
      in 
      div []
         [ Components.button (forward <| update (Save state) state) [] "Save Combo"
         , ComboEditor.thumbEdit mod continue keep
         ]


main = text ""
