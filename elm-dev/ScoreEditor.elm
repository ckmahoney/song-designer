type Editor = 
  EScore Int (Maybe T.Score)
  -- | CreateScore T.Score
  -- | UpdateScore Int (Maybe T.Score)
  -- | DeleteScore T.Score

introduceScore : Html msg -> msg -> Html msg
introduceScore helpLink create =
  Components.box 
    [ p [ class "label" ] [ text "Scores are the actual data for your songs. Recordings, sheet music, MIDI files, stems - anything and everything we have for your music." ]
     , Components.cols 
      [ Components.colHalf <| Components.wraps  
        [ p [] [ text "This one is pretty easy and you can jump right in. We also have a guide for it here." ]
        , helpLink ] 
      , Components.colHalf <| Components.wraps 
         [ p [] [ text "Ready to get going? Click this button to get started now." ] 
        , Components.button create [] "Make a Score"
     ] ] ]


    EScore index mScore ->
     case mScore of  
       -- Checking against making a new score or edit
       Nothing -> 
         case List.length model.scores of 
           0 -> 
            let
               helpLink = Components.button (ChangeView Dash) [] "Read the Guide" 
            in 
             -- create the first score
             introduceScore helpLink (ChangeView newScore)

           len -> 
              -- Editing an existing score           
             if model.index > -1 then
               let
                  deleter = (\v -> (DeleteScore  v))
                  buttDelete = Components.button (deleter score) [ class "has-background-warning" ] "Delete Score!"
                  score = case Tools.get model.index model.scores of
                    Nothing -> -- not going to happen
                      Data.emptyScore

                    Just v ->
                      v
                  uScore = (\s -> (UpdateScore model.index (Just s)))
               in 
                 View.editScore score uScore (text "") buttDelete 
  
             else 
               let
                 children = if List.length model.scores > 3 then 
                     [ p [] [text "Great job! Let's go on to the next part now to make some Scopes." 
                     , Components.button (ChangeView newScope) [] "Goto Scopes"
                     ] ]
                   else
                     [ text <| "You're doing good so far. We need " ++ (String.fromInt (4 - ((List.length model.scores ))) ++ " more scores to start making our scopes." )]
               in
                -- show the picker to select an element to edit
               Components.box <| [ View.scorePicker model.scores Select (ChangeView newScore) ] ++ children

       -- Making a new score
       Just score ->
         let
           buttSave = Components.button (CreateScore score) [] "Save Score"
           buttDelete = Components.button (ChangeView dashboard) [] "Discard"
         in 
         View.editScore score (\v -> ChangeView <| EScore index (Just v)) buttSave buttDelete
