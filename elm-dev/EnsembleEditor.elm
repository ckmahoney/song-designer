type Editor 
  = EEnsemble
  | UpdateEnsembles (List T.Ensemble)  

  -- , ensembles : List TEnsemble

    EEnsemble ->
     let
       curr = Tools.get model.index model.ensembles
       updateCurr = (\ens ->
         case ens of 
           Nothing ->
             ChangeView Dash

           Just e ->
             let
              next = Tools.replaceAt model.index e model.ensembles 
             in 
             UpdateEnsembles next)
     in 
     text "this used to be an ensemble editor"
-- View.ensembleEditor model.voices model.ensembles curr select updateCurr UpdateEnsembles

    UpdateEnsembles es ->
      ({ model | ensembles = es }, Cmd.none )

