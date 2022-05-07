module Editor.SketchEditor exposing (view, main)

import Browser
import Html exposing (Html, button, div, text, label, p, input,b)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onInput)

import Defs exposing (Sketch, Duty(..), Relation(..), RelationRef(..))
import Components.View as View 
import Components.Components as Components

type alias Original = Sketch
type alias Updating = Sketch

type alias Viewer  = (List Original, Maybe Original)
type alias Changer = (List Original, (Original, Updating))


type Change
  = Select Sketch
  | Edit Sketch
  | DoneEditing Sketch
  | NeedsTarget RelationRef
  | SelectTarget RelationRef Sketch
  | CancelTargets
  | DiscardChanges


type Msg
  = View
  | Update Change

type WithPending 
  = PickTarget RelationRef
  | SelectedTarget Sketch

type Editor pending
  = Viewing Viewer
  | Changing Changer (Maybe pending)


-- Defaults to a view to select a sketch to view/edit. 
newEditor : Editor WithPending
newEditor = 
  Viewing (someSketches, Nothing)


init : (Maybe Int) -> (Editor WithPending, Cmd msg)
init flags =
  case flags of 
    _ -> (newEditor, Cmd.none)


someSketches : List Sketch
someSketches =  
  let
    ref = Defs.emptySketch
  in 
  [ { ref | id = 0, label = "sketch 1", duty = Structure}, { ref | id = 1, label="sketch2", duty = Open } , { ref | id = 2, label="sketch 3"} ]
  

applyChange : Change -> (Maybe WithPending) -> (List Sketch, (Sketch, Sketch)) -> Editor WithPending
applyChange msg status (all, (orig, changes)) =
  case msg of 
    Edit next -> Changing (all, (orig, next)) Nothing
    DoneEditing final -> Viewing (all, Just final)

    NeedsTarget relationRef -> 
      case relationRef of 
        JUnique -> Changing (all, (orig, { changes | relation = Unique })) Nothing
        JSource -> Changing (all, (orig, { changes | relation = Source })) Nothing   
        JVariation -> 
          case status of 
            Just (SelectedTarget sketch) -> Changing (all, (orig, { changes | relation = Variation sketch })) Nothing
            _ -> Changing (all, (orig, changes)) (Just <| PickTarget JVariation)

        JClone -> 
          case status of 
            Just (SelectedTarget sketch) -> Changing (all, (orig, { changes | relation = Clone sketch })) Nothing
            _ -> Changing (all, (orig, changes)) (Just <| PickTarget JClone)

    SelectTarget ref sketch ->
      case ref of 
        JVariation ->  Changing (all, (orig, { changes | relation = Variation sketch })) Nothing
        JClone ->  Changing (all, (orig, { changes | relation = Clone sketch })) Nothing
        _ -> Changing (all, (orig, changes)) Nothing

    CancelTargets -> Changing (all, (orig, changes)) Nothing
    DiscardChanges -> Viewing (all, Just orig) 
    _ -> Debug.log "Bug found, needs to apply changes for leftover Changing messages." <| Changing (all, (orig, changes)) Nothing


apply : Msg -> Editor WithPending -> Editor WithPending
apply msg editor =
  case msg of 
    View -> 
     case editor of 
       Viewing (all, Nothing)->  Viewing (all, Nothing)
       Viewing (all, Just sketch)->  Viewing (all, Just sketch)
       Changing (all, (orig, changes)) _ -> Viewing (all, Just changes) 

    Update change  -> 
      case editor of 
        Viewing (all, Nothing) -> 
          case change of 
            Select sketch -> Changing (all, (sketch, sketch)) Nothing
            _ -> Debug.log "Bug found, case change -> viewing -> no selection" <| Viewing (all, Nothing)

        Viewing (all, Just sketch) -> applyChange change Nothing (all, (sketch, sketch))
        Changing (all, (orig, changes)) status -> applyChange change status (all, (orig, changes)) 


update : Msg -> Editor WithPending -> (Editor WithPending, Cmd msg)
update msg editor =
  (apply msg editor, Cmd.none)


dutyInfo : Duty -> Html msg
dutyInfo duty =
  div [Attr.class "column"] 
    [ Components.label <| Defs.dutyLabel duty  
    , Components.paragraph <| Defs.dutyDescr duty 
    ]  

relationInfo : Relation -> Html msg
relationInfo relation =
  div [Attr.class "column"] 
    [ Components.label <| Defs.relationLabel relation
    , Components.paragraph <| Defs.relationDescr relation
    ]  


-- Shows the high level details of a Sketch.
info : Sketch -> Html msg
info sketch  = 
  Components.box <|
    [ Components.label sketch.label
    , Components.cols 
        [ dutyInfo sketch.duty
        , relationInfo sketch.relation
        ]
    , Components.cols 
        [ Components.col1 <| Components.label <| "Size : " ++ String.fromInt sketch.size
        ]
    ]

-- Shows the high level details of a Sketch.
infoEdit : Sketch -> msg -> Html msg
infoEdit sketch doEdit = 
  Components.box <|
    [ Components.label sketch.label
    , Components.cols 
        [ dutyInfo sketch.duty
        , relationInfo sketch.relation
        ]
    , Components.cols 
        [ Components.col1 <| Components.label <| "Size : " ++ String.fromInt sketch.size
        , Components.col1 <| Components.button doEdit []  "Edit this sketch"
        ]
    ]

editDuty : Duty -> (Duty -> msg) -> Html msg
editDuty current change =  
  let
    options = Defs.coreDuties
  in 
  Components.box 
  [ Components.label "Duty"
  , div [ Attr.class "is-multiline level is-vcentered" ] <|
      List.map (\duty ->
       let
         class = if duty == current then "is-success" else "" 
       in 
        Components.button (change duty) [Attr.class class,  Attr.class "has-text-centered"] (Defs.dutyLabel duty)) options
  ]


matchesRelation : Relation -> RelationRef -> Bool
matchesRelation relation ref =
  case relation of 
    Unique -> ref == JUnique
    Source -> ref == JSource
    Variation _ -> ref == JVariation
    Clone _ -> ref == JClone
    -- _ -> False
    -- Connect _ -> ref == JConnect


editRelation : Relation -> (RelationRef -> msg) -> (Sketch -> msg) -> Html msg
editRelation current needOne pickOne =  
  let
    options = Defs.coreRelations
  in 
  Components.box 
  [ Components.label "Relation"
  , div [ Attr.class "is-multiline level is-vcentered" ] <|
      List.map (\ref ->
       let
         class = if matchesRelation current ref then "is-success" else "" 
       in 
       Components.button (needOne ref) [Attr.class class,  Attr.class "has-text-centered"] (Defs.relationLabelJ ref)) options
  , case current of 
      Variation target -> 
        div [] 
          [ Components.paragraph "Making a variation of this sketch"
          , info target 
          ]
      Clone target ->
        div [] 
          [ Components.paragraph "Making a clone of this sketch"
          , info target 
          ]
      Unique ->
        Components.paragraph "This is a one-of-a-kind sketch."

      Source ->
        Components.paragraph "This sketch is a source that can provide Variations and Clones." 
  ]


pickTarget : Sketch -> List Sketch -> RelationRef -> (Sketch -> msg) -> Html msg
pickTarget current sketches relationRef pick =
  Components.box <| 
    [ Components.label "Pick a target for this relation" ] ++  
    List.map (\sketch -> 
      if current.id == sketch.id then text "" else 
      div [onClick (pick sketch)]  [info sketch]) sketches
  


pickSketch : List Sketch -> RelationRef -> Sketch -> (Sketch -> msg) -> msg -> Html msg
pickSketch sketches ref current doEdit cancel =
  let
    elect = (\target -> 
       case ref of 
         JVariation ->  doEdit { current | relation = Variation target }
         JClone ->  doEdit { current | relation = Clone target }
         JUnique ->  doEdit { current | relation = Unique }
         JSource ->  doEdit { current | relation = Source })
  in
  Components.box <|
    [ Components.label "Select a sketch" ] ++
    List.map (\sketch -> 
      if sketch == current then (text "not this one") else  
      div [onClick (elect sketch)] [info sketch]) sketches
   

edit : Sketch -> (Sketch -> msg) -> msg -> msg -> (RelationRef -> msg) -> (Sketch -> msg) -> Html msg
edit changes change save discard needOne pickOne =
  let
    uLabel = (\label -> change { changes | label = label })
    uDuty = (\duty -> change { changes | duty = duty })
  in 
  div [Attr.class "box"] 
    [ Components.cols 
        [ Components.col1 <| Components.editText "Label" (text "") changes.label uLabel
        ]
    , editDuty changes.duty uDuty
    , editRelation changes.relation needOne pickOne
    , Components.col1 <| Components.button save [] "Save Changes"
      , Components.col1 <| Components.button discard []  "Discard Changes"
    ]


sketchPicker : List Sketch -> (Sketch -> msg) -> Html msg
sketchPicker sketches select =
  div [Attr.class "box"] <|
    [ Components.label "Select a Sketch" ] ++
    List.map (\sketch -> 
      div [onClick (select sketch)] [info sketch]) sketches


-- Generic view for seeing and editing a Sketch
view : Editor WithPending -> (Sketch -> msg) -> (Sketch -> msg) -> msg -> msg -> (RelationRef -> msg) -> (Sketch -> msg) -> Html msg
view editor select change save discard needOne pickOne =
  case editor of 
    Viewing (sketches, Nothing) -> sketchPicker sketches select
    Viewing (sketches, Just sketch) -> infoEdit sketch (change sketch)
    Changing (all, (orig, current)) (Just (PickTarget ref)) -> pickTarget orig all ref pickOne
    Changing (all, (orig, current)) _ -> edit current change save discard needOne pickOne


viewTest : Editor WithPending -> Html Msg
viewTest editor =
  let
    select = Update << Select
    change = Update << Edit 
    needOne = Update << NeedsTarget
    pickOne = Update << case editor of 
      Changing _ (Just pending) -> 
        case pending of 
          PickTarget ref ->  SelectTarget ref
          _ -> SelectTarget JVariation
      _ -> SelectTarget JVariation

    save = View
    discard = Update DiscardChanges
  in 
  view editor select change save discard needOne pickOne


main = Browser.element 
  { init = init
  , update = update
  , view = viewTest
  , subscriptions = (\_ -> Sub.none)
  }
