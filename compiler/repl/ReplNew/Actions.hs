{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ReplNew.Actions
  ( doReplAction,
  )
where

import Data.Functor
import Data.Text (Text)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import ReplNew.Actions.Evaluate
import ReplNew.Actions.ListModules
import ReplNew.Helpers
import ReplNew.ReplM
import ReplNew.Types

doReplAction ::
  Project Annotation ->
  Text ->
  ReplAction Annotation ->
  ReplM (Error Annotation) (Project Annotation)
doReplAction env input action =
  case action of
    Help -> do
      doHelp
      pure env
    ListModules ->
      catchMimsaError env (doListModules env input $> env)
    (Evaluate expr) ->
      catchMimsaError env (doEvaluate env input expr $> env)

----------

doHelp :: ReplM e ()
doHelp = do
  replOutput @Text "~~~ MIMSA ~~~"
  replOutput @Text ":help - this help screen"
  replOutput @Text ":modules - show a list of modules in the project"
  replOutput @Text "<expr> - Evaluate <expr>, returning it's simplified form and type"
  replOutput @Text ":quit - give up and leave"

----------
