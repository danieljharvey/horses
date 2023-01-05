{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Repl.Actions
  ( doReplAction,
    doHelp,
  )
where

import Data.Functor
import Data.Text (Text)
import Language.Mimsa.Core
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Repl.Actions.Bindings
import Repl.Actions.Compile
import Repl.Actions.Evaluate
import Repl.Actions.ListModules
import Repl.Helpers
import Repl.ReplM
import Repl.Types

doReplAction ::
  Project Annotation ->
  ReplAction Annotation ->
  ReplM (Error Annotation) (Project Annotation)
doReplAction prj action =
  case action of
    Help -> do
      doHelp
      pure prj
    ListModules modName ->
      catchMimsaError prj (doListModules prj modName $> prj)
    ListBindings ->
      catchMimsaError prj (doListBindings $> prj)
    (Evaluate expr) ->
      catchMimsaError prj (doEvaluate prj expr $> prj)
    (AddBinding modItem) ->
      catchMimsaError
        prj
        ( doAddBinding prj modItem $> prj
        )
    (OutputModuleJS be moduleName) ->
      catchMimsaError prj (doOutputModuleJS prj be moduleName $> prj)

----------

doHelp :: ReplM e ()
doHelp = do
  replOutput @Text "~~~ MIMSA ~~~"
  replOutput @Text ":help - this help screen"
  replOutput @Text ":modules <moduleName> - show a list of modules in the project or details of a module"
  replOutput @Text ":list - show a list of bindings created in this repl session"
  replOutput @Text ":bind <binding> - bind an expression, infix or type"
  replOutput @Text "<expr> - Evaluate <expr>, returning it's simplified form and type"
  replOutput @Text ":compile <typescript|javascript> <moduleName> - compile module"
  replOutput @Text ":quit - give up and leave"

----------
