module GuidedDesigner exposing (..)


import Types as T
import Data
import View 
import Components
import Tools
import Array
import Http
import Url.Builder as Url
import Json.Decode as Decode
import Json.Encode as Encode


-- holds transient data passed as type parameters
type Editor 
  = Dash
  | EScope Int (Maybe T.Scope)
  | EVoice Int (Maybe T.Voice)
  | ELayout Int (Maybe T.Layout)
  | ELayoutEdit T.Layout
  | ELayoutEdit1 T.Layout T.Scope T.Ensemble
  | ETemplate Int (Maybe T.Template)
  
  | EAsset


type model = 
  { view = Editor
  , index : Int
  , voices : List T.Voice
  , scopes : List T.Scope
  , layouts : List T.Layout
  , templates : List T.Template
  } 

scopeDash : Editor
scopeDash = 
  EScope -1 Nothing


voiceDash : Editor
voiceDash = 
  EVoice -1 Nothing


layoutDash : Editor
layoutDash = 
  ELayout -1 Nothing


templateDash : Editor
templateDash = 
  clear ETemplate


type Msg 
  = ChangeView Editor 
  | Select Int

  | CreateVoice T.Voice
  | UpdateVoice Int (Maybe T.Voice)
  | DeleteVoice T.Voice

  | CreateScope T.Scope
  | UpdateScope Int (Maybe T.Scope)
  | DeleteScope T.Scope

  | CreateLayout T.Layout
  | UpdateLayout Int (Maybe T.Layout)  | DeleteLayout T.Layout

  | CreateTemplate T.Template
  | UpdateTemplate Int (Maybe T.Template)
  | DeleteTemplate T.Template




welcome : Model -> Html Msg 
welcome model =
  Html.div [ class "container content" ] 
    [ Html.h3 [ class "subtitle" ] [ text "welcome" ] 
    , Html.p [] [ text "Here is a quick rundown of your orchestration." ]
    , div [ class "columns is-multiline" ] 
      [ panel "Voice" "voices" model.voices (ChangeView (clear EVoice))
      , panel "Scope" "scopes" model.scopes (ChangeView (clear EScope))

      , panel "Layout" "layouts" model.layouts (ChangeView (clear ELayout)) 
      , panel "Template" "templates" model.templates (ChangeView (clear ETemplate))
      ] ]


dashboard : Editor
dashboard = Dash


-- newScore = EScore -1 (Just Data.emptyScore)


newScope = EScope -1 (Just Data.emptyScope)


newVoice = EVoice -1 (Just Data.emptyVoice)


newLayout = ELayout -1 (Just Data.emptyLayout)
newCombo = ELayout -1 (Just Data.emptyLayout)


newTemplate = ETemplate -1 (Just Data.emptyTemplate)



display : Model -> (Int -> Msg) -> Html Msg
display model select =
  case model.view of 
    Dash -> 
     welcome model

          
    EScope index mScope ->
     case mScope of  
       -- Checking against making a new scope or edit
       Nothing -> 
         case List.length model.scopes of 
           0 -> 
            let
               helpLink = Components.button (ChangeView Dash) [] "Read the Guide" 
            in 
             -- create the first scope
             introduceScope helpLink (ChangeView newScope)

           len -> 
              -- Editing an existing scope           
             if model.index > -1 then
               let
                  deleter = (\v -> (DeleteScope  v))
                  buttDelete = Components.button (deleter scope) [ class "has-background-warning" ] "Delete Scope!"
                  scope = case Tools.get model.index model.scopes of
                    Nothing -> -- not going to happen
                      Data.emptyScope

                    Just v ->
                      v
               in 
                 View.editScope scope (UpdateScope model.index) (text "") buttDelete 
  
             else 
               let
                 children = if List.length model.scopes > 2 then 
                     [ p [] [text "You are doing well, so let's move to the next part and make a Layout." 
                     , Components.button (ChangeView layoutDash) [] "Goto Layouts"
                     ] ]
                   else
                     [ text "You're doing good so far. We need one more scope to start making a layout. " ]
               in
                -- show the picker to select an element to edit
               Components.box <| [ View.scopePicker model.scopes Select (ChangeView newScope) ] ++ children


       -- Making a new scope
       Just scope ->
         let
           buttSave = Components.button (CreateScope scope) [] "Save Scope"
           buttDelete = Components.button (ChangeView dashboard) [] "Discard"
         in 
         View.editScope scope (\v -> ChangeView <| EScope index v) buttSave buttDelete

          
    EVoice index mVoice ->
     case mVoice of  
       -- Checking against making a new voice or edit
       Nothing -> 
         case List.length model.voices of 
           0 -> 
            let
               helpLink = Components.button (ChangeView Dash) [] "Read the Guide" 
            in 
             -- create the first voice
             introduceVoice helpLink (ChangeView newVoice)

           len -> 
              -- Editing an existing voice           
             if model.index > -1 then
               let
                  deleter = (\v -> (DeleteVoice  v))
                  buttDelete = Components.button (deleter voice) [ class "has-background-warning" ] "Delete Voice!"
                  voice = case Tools.get model.index model.voices of
                    Nothing -> -- not going to happen
                      Data.emptyVoice

                    Just v ->
                      v
               in 
                 View.editVoice voice (UpdateVoice model.index) (text "") buttDelete 
  
             else 
               let
                 children = if List.length model.voices > 3 then 
                     [ p [] [text "Great job! Let's go on to the next part now to make some Scopes." 
                     , Components.button (ChangeView scopeDash) [] "Goto Scopes"
                     ] ]
                   else
                     [ text <| "You're doing good so far. We need " ++ (String.fromInt (4 - ((List.length model.voices ))) ++ " more voices to start making our scopes." )]
               in
                -- show the picker to select an element to edit
               Components.box <| [ View.voicePicker model.voices Select (ChangeView newVoice) ] ++ children

       -- Making a new voice
       Just voice ->
         let
           buttSave = Components.button (CreateVoice voice) [] "Save Voice"
           buttDelete = Components.button (ChangeView dashboard) [] "Discard"
         in 
         View.editVoice voice (\v -> ChangeView <| EVoice index v) buttSave buttDelete


    ELayout index mLayout ->
     -- Checking against making a new layout or edit existing one
     case mLayout of 
       Nothing -> 
         case List.length model.layouts of 
           -- create the first layout
           0 -> 
            let
               helpLink = Components.button (ChangeView Dash) [] "Read the Guide" 
            in 
             introduceLayout helpLink (ChangeView newLayout)

           -- Editing an existing layout           
           len -> 
             if model.index > -1 then
               let
                  deleter = (\v -> (DeleteLayout  v))
                  buttDelete = Components.button (deleter layout) [ class "has-background-warning" ] "Delete Layout!"
                  ((pLabel, pEns) as layout) = case Tools.get model.index model.layouts of
                    Nothing -> -- not going to happen
                      Data.emptyLayout

                    Just v ->
                      v

                  uLayout = (\l ->
                    (ChangeView <| ELayout model.index (Just l)))
                  editComboP = (\cp ->
                    ChangeView <| ELayoutEdit layout)
                  updateCombos = (\next ->
                    UpdateLayout model.index (Just (pLabel, next)))
               in 
               View.designLayout layout model.scopes model.voices uLayout editComboP updateCombos (text "save button") buttDelete 
  
             -- show the picker to select an element to edit     
             else 
               let
                 children = if List.length model.layouts > 0 && List.length model.templates == 0 then 
                     [ p [] [text "Now that you have a layout, let's turn it into a Template so we can start making songs!" ]
                     , Components.button (ChangeView templateDash) [] "Goto Templates"
                     ]
                   else
                     [ text "" ]
               in
               Components.box <| ([ View.layoutPicker model.layouts select (ChangeView newLayout) ] ++ children)


       -- Making a new layout
       Just ((pScope, pEns) as layout) ->
         let
           buttSave = Components.button (CreateLayout layout) [] "Clone Layout"
           buttDelete = Components.button (ChangeView dashboard) [] "Discard"
           uLayout = (\l ->
             (ChangeView <| ELayout model.index (Just l)))
           editComboP = (\cp ->
             ChangeView <| ELayoutEdit layout)
           updateCombos = (\next ->
             UpdateLayout model.index (Just (pScope, next)))
         in 
         View.designLayout layout model.scopes model.voices uLayout editComboP updateCombos  buttSave buttDelete 

                             
    ELayoutEdit layout ->
      let
         pick = (\int ->
           case Tools.get int model.scopes of 
             Nothing -> 
               ChangeView <| ELayoutEdit layout
             Just s -> 
               ChangeView <| ELayoutEdit1 layout s [])
      in 
      Components.box      
        [ p [] [ text "Choose a Scope for this combo." ]
        , Components.picker model.scopes View.scopeIcon pick
        ]


    ELayoutEdit1 ((name, pCombos) as layout) scope voices->
      let
         curr = (name, Tools.conj (scope, voices) pCombos)
         pick = (\int ->
           case Tools.get int model.voices of 
             Nothing -> 
               ChangeView (ELayoutEdit1 layout scope voices)
             Just v -> 
               ChangeView (ELayoutEdit1 layout scope (Tools.conj v voices)))

         kill = (\int ->
           ChangeView (ELayoutEdit1 layout scope (Tools.removeAt int voices)))
      in 
      Components.box
        [ p [] [ text "Add voices to this combo.", Components.button (UpdateLayout model.index (Just curr)) [] "Save" ]
        , Components.picker model.voices View.voiceIcon pick
        , Components.killer voices View.voiceIcon kill
        ]
 

    ETemplate index mTemplate ->
     case mTemplate of  
       -- Checking against making a new template or edit
       Nothing -> 
         case List.length model.templates of 
           0 -> 
            let
               helpLink = Components.button (ChangeView Dash) [] "Read the Guide" 
            in 
             -- create the first template
             introduceTemplate helpLink (ChangeView newTemplate)

           len -> 
              -- Editing oan existing template           
             if model.index > -1 then
               let
                  deleter = (\v -> (DeleteTemplate  v))
                  buttDelete = Components.button (deleter template) [ class "has-background-warning" ] "Delete Template!"
                  sig = (\t -> (UpdateTemplate model.index (Just t)))
                  template = case Tools.get model.index model.templates of
                    Nothing -> -- not going to happen
                      Data.emptyTemplate

                    Just v ->
                      v
               in 
                 View.editTemplate template model.scopes model.voices sig (text "editing an existing template") buttDelete 
  
             else 
               let
                children = if List.length model.templates > 0 then 
                     [ p [] [text "Amazing. You have made everything you need to produce your first song. Go ahead. Try it out." 
                     , Components.button (ChangeView Dash) [] "Goto Scores"
                     ] ]
                   else
                     [ text "This is the last stop on our tour of the Song Designer. Once you finish creating a Template, you will then be able to make a Score (and infinite variations). So what are you waiting for?! Get Designing!!" ]
               in
                -- show the picker to select an element to edit
               Components.box <|
                 [ View.templatePicker model.templates Select (ChangeView newTemplate) ] ++ children


       -- Making a new template
       Just template ->
         let
           buttSave = Components.button (CreateTemplate template) [] "Save Template"
           buttDelete = Components.button (ChangeView dashboard) [] "Discard"
           sig = (\t -> (ChangeView <| ETemplate index (Just t)))
         in 
         View.editTemplate template model.scopes model.voices sig buttSave buttDelete

    EAsset ->
      View.asset


menuItems : List (String, Editor)
menuItems = 
  [ ("Dash", Dash)
  , ("Voices", clear EVoice)
  , ("Scopes", clear EScope)
  , ("Layouts", clear ELayout)
  , ("Templates", templateDash)
  ] 


menuItem : Bool -> String -> msg -> Html msg
menuItem isSelected label msg =
  Html.li [ onClick msg, class "has-background-white",  class <| if isSelected then "is-active" else "" ]
   [ Html.a [] [  text label ] ]


mmenu : Editor -> List (String, Editor) -> (Editor -> Msg) -> Html Msg
mmenu current items caller =
  div [ class "tabs is-toggle is-toggle-rounded"]
    [ Html.ul [] 
      (List.map (\(label, v) -> 
        menuItem (v == current) label (caller v)) items) ]


menu : Editor ->  Html Msg
menu curr=
  mmenu curr menuItems ChangeView



update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    ChangeView v ->
     ({ model | view = v, index = -1 }, Cmd.none)

    Select i ->
      case i < 0 of 
        True -> ({ model | index = -1 }, Cmd.none) -- indicates no selection
        False -> ({ model | index = i }, Cmd.none) -- has selection for current Edit state

    CreateScope scope ->
      ({ model 
       | scopes = Tools.conj scope model.scopes
       , index = -1
       , view = Dash 
      }, Cmd.none)

    CreateVoice voice ->
      ({ model 
       | voices = Tools.conj voice model.voices
       , index = -1
       , view = Dash 
      }, Cmd.none)

    CreateTemplate template ->
      ({ model 
       | templates = Tools.conj template model.templates
       , index = -1
       , view = Dash 
      }, Cmd.none)

    DeleteTemplate template ->
      ({ model 
      | index = -1
      , templates = Tools.remove (Just template) model.templates }, Cmd.none )

    UpdateScope index mScope ->
      case mScope of 
        Nothing ->
          ({ model 
           | scopes = Tools.removeAt index model.scopes
           , index = -1
           , view = scopeDash }, Cmd.none)

        Just scope ->
          ({ model 
           | scopes = Tools.replaceAt model.index scope model.scopes 
           , view = scopeDash }, Cmd.none)

    DeleteScope scope ->
      ({ model 
       | index = -1
       , scopes = Tools.remove (Just scope) model.scopes }, Cmd.none )

    CreateLayout layout ->
      ({ model 
       | layouts = Tools.conj layout model.layouts
       , index = -1
       , view = Dash 
      }, Cmd.none)

    UpdateLayout index mLayout ->
      let
        foundIndex = Tools.findIndex mLayout (List.map Just model.layouts)
        i = if foundIndex == -1 then index else foundIndex
      in
      case mLayout of 
        Nothing ->
          ({ model 
           | layouts = Tools.removeAt i model.layouts
           , index = -1
           , view = layoutDash }, Cmd.none)

        Just layout ->
          if -1 == index then  
            ({ model 
             | layouts = [ layout ] 
             , index = -1
             , view = layoutDash }, Cmd.none)

          else 
            ({ model 
             | layouts = Tools.replaceAt i layout model.layouts 
             , view = ELayout model.index (Just layout) }, Cmd.none)

    DeleteLayout layout ->
      ({ model 
       | index = -1
       , layouts = Tools.remove (Just layout) model.layouts }, Cmd.none )

    UpdateVoice index mVoice ->
      case mVoice of 
        Nothing ->
          ({ model 
           | voices = Tools.removeAt index model.voices
           , index = -1
           , view = voiceDash }, Cmd.none)

        Just voice ->
          ({ model 
           | voices = Tools.replaceAt model.index voice model.voices 
           , view = voiceDash }, Cmd.none)

    DeleteVoice voice ->
      ({ model 
       | index = -1
       , voices = Tools.remove (Just voice) model.voices }, Cmd.none )

    UpdateTemplate index mTemplate ->
      case mTemplate of 
        Nothing ->
          ({ model
           | templates = Tools.removeAt index model.templates
           , index = -1
           , view = templateDash }, Cmd.none)

        Just template ->
          ({ model 
           | templates = Tools.replaceAt model.index template model.templates 
           , view = templateDash }, Cmd.none)
