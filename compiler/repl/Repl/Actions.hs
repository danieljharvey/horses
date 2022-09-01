{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Repl.Actions
  ( doReplAction,
  )
where

import Data.Functor
import Data.Text (Text)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Repl.Actions.Compile
import Repl.Actions.Evaluate
import Repl.Actions.ExpressionBind
import Repl.Actions.Info
import Repl.Actions.ListBindings
import Repl.Actions.TypeSearch
import Repl.Helpers
import Repl.ReplM
import Repl.Types

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
    ListBindings ->
      catchMimsaError env (doListBindings env input $> env)
    (Evaluate expr) ->
      catchMimsaError env (doEvaluate env input expr $> env)
    (Info expr) ->
      catchMimsaError env (doInfo env input expr $> env)
    (Bind name expr) ->
      catchMimsaError env (doBind env input name expr)
    (BindType dt) ->
      catchMimsaError env (doBindType env input dt)
    (OutputJS be expr) ->
      catchMimsaError env (doOutputJS env input be expr $> env)
    (TypeSearch mt) ->
      catchMimsaError env (doTypeSearch env mt $> env)

----------

doHelp :: ReplM e ()
doHelp = do
  replOutput @Text "~~~ MIMSA ~~~"
  replOutput @Text ":help - this help screen"
  replOutput @Text ":info <expr> - get the type of <expr>"
  replOutput @Text ":bind <name> = <expr> - binds <expr> to <name> and saves it in the environment"
  replOutput @Text ":bindType type Either a b = Left a | Right b - binds a new type and saves it in the environment"
  replOutput @Text ":list - show a list of current bindings in the environment"
  replOutput @Text ":outputJS <javascript|typescript> <expr> - save JS code for <expr>"
  replOutput @Text ":search <mt> - search for exprs that match type"
  replOutput @Text "<expr> - Evaluate <expr>, returning it's simplified form and type"
  replOutput @Text ":quit - give up and leave"

----------
