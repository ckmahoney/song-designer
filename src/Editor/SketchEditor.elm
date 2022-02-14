module Editor.SketchEditor exposing (view, main)

import Browser
import Html exposing (Html, button, div, text, label, p, input,b)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick, onInput)

import Defs exposing (Sketch, Duty(..), Relation(..), RelationRef(..))
import View 
import Elements
import Tools
import Components

type alias Original = Sketch
type alias Updating = Sketch

type alias Model = (Original, Maybe Updating)


type Msg 
  = PickRelation RelationRef
  | CancelPick
  | Edit Sketch
  | Save
  | Discard


type Editor 
  = View Model
  | Editing Model
  | PickingSketch (List Sketch) RelationRef Model 


someSketches : List Sketch
someSketches =  
  let
    ref = Defs.emptySketch
  in 
  [ { ref | label = "sketch1"}, { ref | label="sketch2"} ]
  

-- Pure update given a Msg and Sketch
apply : Msg -> Sketch -> Sketch
apply msg sketch =
  case msg of 
    Edit next -> next
    _ -> sketch


-- Updates that close the editor and save a final copy of Sketch, or initialize edit state
upView : Msg -> Sketch -> (Editor, Cmd msg)
upView msg sketch =
  case msg of 
    Edit _ -> (Editing (sketch, Just sketch), Cmd.none)

    _ -> (View (sketch, Nothing), Cmd.none)

-- Updates with an open editor and reference to before/after changes
upEditing : Msg -> (Sketch, Sketch) -> (Editor, Cmd msg)
upEditing msg (orig, changes) =
  case msg of   
    PickRelation ref -> 
      case ref of 
        JUnique -> (Editing (orig, Just { changes | relation = Unique }), Cmd.none)
        JSource -> (Editing (orig, Just { changes | relation = Source }), Cmd.none)
        _ -> 
         (PickingSketch someSketches ref (orig, Just changes), Cmd.none)

    Save -> (View (changes, Nothing), Cmd.none)
    Discard -> (View (orig, Nothing), Cmd.none)

    _ -> (Editing (orig, Just <| apply msg changes), Cmd.none)


upPicking : Msg -> RelationRef -> (Sketch, Sketch) -> (Editor, Cmd msg)
upPicking msg ref (orig, changes) =
  case msg of 
    CancelPick -> 
      (Editing  (orig, Just changes), Cmd.none)

    Edit sketch -> 
      (Editing (orig, Just sketch), Cmd.none)

    _ ->       (Editing  (orig, Just changes), Cmd.none)




-- Update with possible side effects. Defaults to a pure update.
update : Msg -> Editor -> (Editor, Cmd msg)
update msg editor =
  case editor of 
    View (sketch, _) -> upView msg sketch
    Editing (orig, Just changes) -> upEditing msg (orig, changes)
    PickingSketch _ ref (orig, Just changes) -> upPicking msg ref (orig, changes)
    _ -> (editor, Cmd.none)


initEditor : Editor
initEditor = 
  View (Sketch "New Sketch" Declare Source 2, Nothing)


init : (Maybe Int) -> (Editor, Cmd msg)
init flags =
  (initEditor, Cmd.none)


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
    _ -> False
    -- Connect _ -> ref == JConnect


editRelation : Relation -> (RelationRef -> msg) -> Html msg
editRelation current change =  
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
       Components.button (change ref) [Attr.class class,  Attr.class "has-text-centered"] (Defs.relationLabelJ ref)) options
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

      _ -> 
        Components.paragraph "This sketch makes a bridget between these other skethes:"
  ]


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
     


edit : Sketch -> Sketch -> (Sketch -> msg) -> (RelationRef -> msg) -> msg -> msg -> Html msg
edit original changes change pick save discard =
  let
    uLabel = (\label -> change { changes | label = label })
    uDuty = (\duty -> change { changes | duty = duty })
  in 
  div [Attr.class "box"] 
    [ Components.cols 
        [ Components.col1 <| Components.editText "Label" (text "") changes.label uLabel
        ]
    , editDuty changes.duty uDuty
    , editRelation changes.relation pick
    , Components.col1 <| Components.button save [] "Save changes"
      , Components.col1 <| Components.button discard []  "Discard changes"
    ]


-- Generic view for editing a sketch
view : Editor ->  (Sketch -> msg) -> msg -> msg -> (RelationRef -> msg) -> msg -> Html msg
view model doEdit doSave doDiscard doPick doCancel = 
  case model of 
    View (orig, Nothing) -> infoEdit orig (doEdit orig)
    Editing (orig, Just changes) -> edit orig changes doEdit doPick doSave doDiscard
    PickingSketch sketches ref (orig, Just changes) -> pickSketch sketches ref changes doEdit doCancel 
    _ -> text "unhandled view"

-- View with local messages provided
viewTest : Editor -> Html Msg
viewTest model =
  let
    doEdit = Edit
    doSave = Save
    doDiscard = Discard
    doPick = PickRelation
    cancel = CancelPick
  in 
  view model doEdit doSave doDiscard doPick cancel


main = Browser.element 
  { init = init
  , update = update
  , view = viewTest
  , subscriptions = (\_ -> Sub.none)
  }
