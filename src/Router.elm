port module Router exposing (..)

import Browser
import Html exposing (Html, button, div, text, label, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

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

import ScopeEditor
import EnsembleEditor
import ComboEditor
import LayoutEditor
import MiniSongDesigner

port playMusic : String -> Cmd msg

port pauseMusic : String -> Cmd msg

port stopMusic : String -> Cmd msg

port setSource : String -> Cmd msg



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


type Playback
 = Play
 | Pause
 | Stop  

-- data prepared for storage on server
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
  | UpdateLayout Int (Maybe T.Layout)
  | DeleteLayout T.Layout

  | CreateTemplate T.Template
  | UpdateTemplate Int (Maybe T.Template)
  | DeleteTemplate T.Template

  | LoadTrack T.TrackMeta
  | PlayTrack 
  | PauseTrack
  | StopTrack
  | SelectTrack (Maybe T.TrackMeta)
 
  | GotTracks (Result Http.Error (List T.TrackMeta))
  | GotNewTrack (Result Http.Error  T.TrackMeta)
  | ReqTrack T.Template
  | GotResp (Result Http.Error String)
  
  | OpenLayoutEditor (List T.Combo)
  | CloseLayoutEditor (List T.Combo)
  | SelectLayoutEditor (List T.Combo) Int T.Combo
  | UpdateLayoutEditor (List T.Combo) Int LayoutEditor.EditState
  | UpdateTitle (String)


loader = MiniSongDesigner.initModel


type alias Model =
  { view : Editor
  , index : Int
  , voices : List T.Voice
  , scopes : List T.Scope
  , layouts : List T.Layout
  , templates : List T.Template
  , response : String
  , mailer : T.Posting
  , tracks : List T.TrackMeta
  , selection : Maybe T.TrackMeta
  , playstate : Playback
  , member : Maybe T.GhostMember
  , layout : List T.Combo
  , layoutEditor : Maybe LayoutEditor.Model
  , title : String
  }


decodeTrackPrev : Decode.Decoder T.Track
decodeTrackPrev =
  Decode.map4 T.Track
    (Decode.field "id" Decode.int)
    (Decode.field "src" Decode.string)
    (Decode.field "size" Decode.int)
    (Decode.field "duration" Decode.float)


decodeTrack : Decode.Decoder T.TrackMeta
decodeTrack =
  Decode.map6 T.TrackMeta
    (Decode.field "id" Decode.int)
    (Decode.field "account_id" Decode.int)
    (Decode.field "filepath" Decode.string)
    (Decode.field "title" Decode.string)
    (Decode.field "size_bytes" Decode.int)
    (Decode.field "duration_seconds" Decode.float)


initFrom : List T.Voice -> List T.Scope -> List T.Layout -> List T.Template -> Maybe T.GhostMember -> Model
initFrom v s l t m =
  Model newLayout -1 Data.kitAll Data.scopes3 l t "" T.Welcome  [] Nothing Stop m LayoutEditor.initState Nothing ""


initTest : Model
initTest = 
  initFrom [Data.p1, Data.p2] [] [] []  Nothing


initEmpty : Model
initEmpty = 
  Model Dash -1 [] [] [] [] "" T.Welcome  [] Nothing Stop Nothing LayoutEditor.initState Nothing ""


initFromMember : T.GhostMember -> Model
initFromMember member = 
  let
    rec = { initEmpty | member = Just member }
  in 
  { initTest | member = Just member }


init : Maybe T.GhostMember -> (Model, Cmd Msg)
init flags =
  case flags of 
    Nothing -> 
      (initEmpty, Cmd.none)
    
    Just member ->
      (initFromMember member, getSongs member.email member.uuid)


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


apiUrl : String -> String 
apiUrl endpoint =
  Url.crossOrigin "http://localhost:3000" [ endpoint ] []


reqTrack : String -> String -> T.Template -> Cmd Msg
reqTrack email uuid template =
  Debug.log "Posting:" <| 
  Http.post
    { url = apiUrl "track"
    , body = Debug.log "sending body:" <| Http.jsonBody <| encodeReqTrack email uuid template
    , expect = Http.expectJson GotNewTrack decodeTrack
    }

reqMiniTrack : String -> String -> T.Layout -> Cmd Msg
reqMiniTrack email uuid ((title, combos) as layout) =
 let   
  -- meta = {Data.scoreMetaT0 | title = title }
  template = (Data.scoreMetaT0, layout)
 in
  Http.post
  { url = apiUrl "track"
  , body = Http.jsonBody <| encodeReqTrack email uuid template
  , expect = Http.expectJson GotNewTrack decodeTrack
  }


getSongs : String -> String -> Cmd Msg
getSongs email uuid =
  Http.post
  { url = apiUrl "user"
  , body = Http.jsonBody <| encodeReqLoadSongs email uuid
  , expect = Http.expectJson GotTracks (Decode.list decodeTrack)
  }


encodeScope : T.Scope -> Encode.Value
encodeScope {label, cps, cpc, root, size} =
  Encode.object
    [ ("label", Encode.string label)
    , ("cps", Encode.float cps)
    , ("root", Encode.float <| toFloat root)
    , ("cpc", Encode.int cpc)
    , ("size", Encode.int size)
    ]


encodeVoice : T.Voice -> Encode.Value
encodeVoice {duty, role, label, voice, density, complexity} =
  Encode.object
    [ ("duty", Encode.string <| Data.dutyString duty)
    , ("role", Encode.string <| Data.roleId role)
    , ("label", Encode.string label)
    , ("voice", Encode.int voice)
    , ("density", Encode.int density)
    , ("complexity", Encode.int complexity)
    ]


encodeEnsemble : T.Ensemble -> Encode.Value
encodeEnsemble  =
  Encode.list encodeVoice 


encodeScoreMeta : T.ScoreMeta -> Encode.Value
encodeScoreMeta {title, cps, root, cpc} =
  Encode.object
    [ ("title", Encode.string title)
    , ("cps", Encode.float cps)
    , ("root", Encode.float root)
    , ("cpc", Encode.int cpc)
    ]


encodeCombo : T.Combo -> Encode.Value
encodeCombo (scope, ensemble) =
  Encode.object
    [ ("scope", encodeScope scope) 
    , ("ensemble", encodeEnsemble ensemble)
    ]


encodeLayout : T.Layout -> Encode.Value
encodeLayout (label, combos) =
  Encode.object
    [ ("title", Encode.string label)
    , ("combos", Encode.list encodeCombo combos)
    ]


encodeReqTrack : String -> String -> T.Template -> Encode.Value
encodeReqTrack email uuid template =
  Encode.object
    [ ("meta", encodeScoreMeta <| Tuple.first template)
    , ("layout", encodeLayout <| Tuple.second template)
    , ("email", Encode.string email)
    , ("uuid", Encode.string uuid)
    ]


encodeMember : T.GhostMember -> Encode.Value
encodeMember member =
  Encode.object
    [ ("name", Encode.string member.name)
    , ("email", Encode.string member.email)
    , ("uuid", Encode.string member.uuid)
    ]


encodeUserReq :  Encode.Value
encodeUserReq  =  Encode.object
    [ ("action", Encode.string "songs")
    , ("username", Encode.string "maxwell")
    ]


encodeReqLoadSongs : String -> String -> Encode.Value
encodeReqLoadSongs email uuid = Encode.object
    [ ("action", Encode.string "songs")
    , ("email", Encode.string email)
    , ("uuid", Encode.string uuid)
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateTitle title ->
      ({ model | title = title }, Cmd.none)

    CloseLayoutEditor layout ->
      ({ model | layout = layout, layoutEditor = Nothing }, Cmd.none)

    OpenLayoutEditor layout ->
      ({ model |  layoutEditor = Just <| LayoutEditor.Overview layout}, Cmd.none)

    SelectLayoutEditor layout index combo ->
      ({ model | layoutEditor = Just <| LayoutEditor.Editing layout index (LayoutEditor.Open combo) }, Cmd.none)

    UpdateLayoutEditor layout index editState ->
      case editState of 
        LayoutEditor.Open combo ->
         let
           next = Tools.replaceAt index combo layout
         in
          ({ model | layoutEditor = Just <| LayoutEditor.Editing next index (LayoutEditor.Open combo)}, Cmd.none)

        LayoutEditor.Scope editor ->         
            ({ model | layoutEditor = Just <| LayoutEditor.Editing layout index <| LayoutEditor.Scope editor}, Cmd.none)

        LayoutEditor.Ensemble editor ->
          case editor of 
            EnsembleEditor.Overview ensemble ->
              ({ model | layoutEditor = Just <| LayoutEditor.Editing layout index <| LayoutEditor.Ensemble  editor}, Cmd.none)

            EnsembleEditor.Editing ensemble voiceIndex voice ->
              ({ model | layoutEditor = Just <| LayoutEditor.Editing layout index <| LayoutEditor.Ensemble  editor}, Cmd.none)


    LoadTrack track -> 
      ( model, setSource track.filepath )

    SelectTrack track ->
      case track of 
        Nothing -> 
          ( { model | playstate = Stop, selection = Nothing }, setSource "" )
        Just t -> 
          ( { model | playstate = Play, selection = Just t }, setSource ("http://localhost:3000/" ++ t.filepath ))

    PlayTrack ->
      ( { model | playstate = Play } , playMusic "" )

    PauseTrack ->
      ( { model | playstate = Pause } , pauseMusic "" )

    StopTrack ->
      ( { model | playstate = Stop, selection = Nothing }, stopMusic "" )

    GotTracks response ->
      case response of 
        Ok tracks ->
         ({ model | mailer = T.Received, tracks = tracks }, Cmd.none)

        Err errr ->
          case errr of 
            Http.BadBody str -> 
              ({ model | mailer = T.Failed str }, Cmd.none)
 
            Http.BadUrl str -> 
              ({ model | mailer = T.Failed str }, Cmd.none)

            Http.BadStatus int -> 
              ({ model | mailer = T.Failed <| String.fromInt int }, Cmd.none)

            _ -> 
              ({ model | mailer = T.Failed "big bug" }, Cmd.none)

    GotNewTrack response ->
      case response of 
        Ok track ->
          ( { model 
            | tracks = track :: model.tracks
            , selection = (Just track)
            , playstate = Play
            , mailer = T.Received }, setSource ("http://localhost:3000/" ++ track.filepath ) )

        Err errr ->
         let
           yy = Debug.log "Bad response. This is the raw err:" errr
         in
          case errr of 
            Http.BadBody str -> 
              ({ model | mailer = T.Failed str }, Cmd.none)
 
            Http.BadUrl str -> 
              ({ model | mailer = T.Failed str }, Cmd.none)

            Http.BadStatus int -> 
              ({ model | mailer = T.Failed <| String.fromInt int }, Cmd.none)

            _ -> 
              ({ model | mailer = T.Failed "big bug" }, Cmd.none)

    ReqTrack template -> 
      case model.member of 
        Nothing -> 
         let
          member = Debug.log "Using the test member:" Data.testMember
         in
          ( { model | mailer = T.Sending }, reqTrack member.email member.uuid template )
          -- ( model, Cmd.none )

        Just member ->
          ( { model | mailer = T.Sending }, reqTrack member.email member.uuid template )

    GotResp result -> 
      case result of 
        Ok str -> 
          ({ model | response = str }, Cmd.none)

        Err _ -> 
          ({ model | response = "We had a problem getting your string." }, Cmd.none)

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
    
    ChangeView v ->
     ({ model | view = v, index = -1 }, Cmd.none)

    Select i ->
      case i < 0 of 
        True -> ({ model | index = -1 }, Cmd.none) -- indicates no selection
        False -> ({ model | index = i }, Cmd.none) -- has selection for current Edit state

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


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


toText : Int -> Html msg
toText x =
  text <| String.fromInt x


listLenText : List a -> String
listLenText xs = 
  String.fromInt <| List.length xs


panel : String -> String -> List a -> msg -> Html msg
panel name plural coll msg =
  div [ class "column" ] [ Components.card name <| 
    div [] 
    [ p [] [ text ("You have " ++ (listLenText coll) ++ " " ++ plural) ] 
    , Components.button msg [] ( name ++ " Overview")
    ] ]


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


introduceVoice : Html msg -> msg -> Html msg
introduceVoice helpLink create =
  Components.box 
    [ p [ class "label" ] [ text "You need some voices to make a template. We recommend 4 unique voices to get started." ]
     , Components.cols 
      [ Components.colHalf <| Components.wraps  
        [ p [] [ text "Do you want some help making your first voices and template? Click here for the guide." ]
        , helpLink ] 
      , Components.colHalf <| Components.wraps 
         [ p [] [ text "Ready to get going? Click this button to get started now." ] 
        , Components.button create [] "Make a Voice"
     ] ] ]


introduceScope : Html msg -> msg -> Html msg
introduceScope helpLink create =
  Components.box 
    [ p [ class "label" ] [ text "Scopes let us build Layouts. We recommend 2 unique scopes to get started, but you can always make as many as you want." ]
     , Components.cols 
      [ Components.colHalf <| Components.wraps  
        [ p [] [ text "Do you want some help making your first scope and layout? Click here for the guide." ]
        , helpLink ] 
      , Components.colHalf <| Components.wraps 
         [ p [] [ text "Ready to get going? Click this button to get started now." ] 
        , Components.button create [] "Make a Scope"
     ] ] ]


introduceLayout : Html msg -> msg -> Html msg
introduceLayout helpLink create =
  Components.box 
    [ p [ class "label" ] [ text "Layouts are the fundamental structure of your composition." ]
     , Components.cols 
      [ Components.colHalf <| Components.wraps  
        [ p [] [ text "We recommend everybody read the guide for Layouts before making one." ]
        , helpLink ] 
      , Components.colHalf <| Components.wraps 
         [ p [] [ text "Ready to get going? Click this button to get started now." ] 
        , Components.button create [] "Make a Layout"
     ] ] ]


introduceTemplate : Html msg -> msg -> Html msg
introduceTemplate helpLink create =
  Components.box 
    [ p [ class "label" ] [ text "Templates are the seeds for your flowers of music." ]
    , p [ class "label" ] [ text "Every template can produce an infinite number of songs, where each song similar to its siblings in some ways while being unique." ]
     , Components.cols 
      [ Components.colHalf <| Components.wraps  
        [ p [] [ text "We recommend everybody read the guide for Templates before making one." ]
        , helpLink ] 
      , Components.colHalf <| Components.wraps 
         [ p [] [ text "Ready to get going? Click this button to get started now." ] 
        , Components.button create [] "Make a Template"
     ] ] ]


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


clear msg =
  msg -1 Nothing


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


updateIn : a -> List a -> Int -> List a 
updateIn el els index =
  Tools.replaceAt index el els


playlist : Playback -> (Maybe T.TrackMeta) -> List T.TrackMeta -> Html Msg
playlist playstate selection tracks =
  Components.box <| 
   List.map (\track ->
     let 
       icons = case selection of 
         Nothing -> 
            [ div [onClick <| SelectTrack (Just track)] [Components.svg "play"] ]

         Just selected ->  
           if selected == track then 
             [ case playstate of 
                 Play -> 
                   div [onClick PauseTrack] [Components.svg "pause"] 

                 Pause -> 
                   div [onClick PlayTrack] [Components.svg "play"] 

                 Stop ->
                   div [onClick PlayTrack] [Components.svg "play"] 

             , div [onClick StopTrack] [Components.svg "stop"]
             ]
           else 
             [ div [onClick <| SelectTrack (Just track)] [Components.svg "play"] ]

     in
     Components.songCard track.title icons) tracks


view : Model -> Html Msg
view model =
    div [ class "section" ]
      [ case model.member of 
          Nothing -> text ""
          Just m -> Html.h2 [class "subtitle"] [text ("Welcome back " ++ m.firstname)]
      , menu model.view
      , case model.layoutEditor of 
          Nothing -> 
           Components.button (OpenLayoutEditor model.layout) [] "Edit your layout"

          Just lModel ->
            case lModel of 
              LayoutEditor.Overview layout -> 
               div [] 
                [ Components.button (CloseLayoutEditor layout) [] "Close"
                , LayoutEditor.view layout (\i -> 
                    SelectLayoutEditor layout i <| Tools.getOr i layout Data.emptyCombo) (\i -> OpenLayoutEditor (Tools.removeAt i layout))
                , if 2 > (List.length layout) then 
                     Components.button (OpenLayoutEditor <| List.reverse <| Data.emptyCombo :: layout) [] "Add another Combo" else text ""
                ]

              LayoutEditor.Editing layout index stateModel -> 
               let
                 up = UpdateLayoutEditor layout index
                 combo = Tools.getOr index layout Data.emptyCombo
                 done = OpenLayoutEditor layout
               in
                div []
                  [ Components.button (CloseLayoutEditor layout) [] "Close"
                  , Components.editText "Title" (text "The name for this sound") model.title UpdateTitle
                  , LayoutEditor.edit stateModel index combo up done
                  ]

      , if 0 == List.length model.layout then 
           text ""
        else 
          let 
             ref = Data.scoreMetaT0
             meta = { ref | title = model.title }
             template = (meta, ("Needs a title", model.layout))
          in  
           Components.button (ReqTrack template) [] "Request a Song"

      , case model.mailer of 
          T.Sending -> 
            text "Working on that track for you!"
          _ ->
            display model Select 
      , text model.response
      , playlist model.playstate model.selection model.tracks
      ]


main =  Browser.element 
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }
