{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Language.Mimsa.Repl.Actions
  ( doReplAction,
  )
where

import Data.Functor
import Data.Text (Text)
import Language.Mimsa.Monad
import Language.Mimsa.Repl.Actions.Compile
import Language.Mimsa.Repl.Actions.Evaluate
import Language.Mimsa.Repl.Actions.ExpressionBind
import Language.Mimsa.Repl.Actions.Info
import Language.Mimsa.Repl.Actions.ListBindings
import Language.Mimsa.Repl.Actions.Tree
import Language.Mimsa.Repl.Actions.TypeSearch
import Language.Mimsa.Repl.Actions.UnitTests
import Language.Mimsa.Repl.Actions.Versions (doVersions)
import Language.Mimsa.Repl.Helpers
import Language.Mimsa.Repl.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project

doReplAction ::
  Project Annotation ->
  Text ->
  ReplAction Annotation ->
  MimsaM (Error Annotation) (Project Annotation)
doReplAction env input action =
  case action of
    Help -> do
      doHelp
      pure env
    ListBindings ->
      catchMimsaError env (doListBindings env input $> env)
    (Versions name) ->
      catchMimsaError env (doVersions env name $> env)
    (Evaluate expr) ->
      catchMimsaError env (doEvaluate env input expr $> env)
    (Tree expr) ->
      catchMimsaError env (doTree env input expr $> env)
    (Graph expr) ->
      catchMimsaError env (doGraph env input expr $> env)
    ProjectGraph ->
      catchMimsaError env (doProjectGraph env $> env)
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
    (AddUnitTest testName testExpr) ->
      catchMimsaError env (doAddUnitTest env input testName testExpr)
    (ListTests maybeName) ->
      catchMimsaError env (doListTests env maybeName $> env)

----------

doHelp :: MimsaM e ()
doHelp = do
  replOutput @Text "~~~ MIMSA ~~~"
  replOutput @Text ":help - this help screen"
  replOutput @Text ":info <expr> - get the type of <expr>"
  replOutput @Text ":bind <name> = <expr> - binds <expr> to <name> and saves it in the environment"
  replOutput @Text ":bindType type Either a b = Left a | Right b - binds a new type and saves it in the environment"
  replOutput @Text ":list - show a list of current bindings in the environment"
  replOutput @Text ":outputJS <javascript|typescript> <expr> - save JS code for <expr>"
  replOutput @Text ":tree <expr> - draw a dependency tree for <expr>"
  replOutput @Text ":graph <expr> - output graphviz dependency tree for <expr>"
  replOutput @Text ":search <mt> - search for exprs that match type"
  replOutput @Text ":addTest \"<test name>\" <expr> - add a unit test"
  replOutput @Text ":listTests <optional name> - list tests for <name>"
  replOutput @Text ":versions <name> - list all versions of a binding"
  replOutput @Text "<expr> - Evaluate <expr>, returning it's simplified form and type"
  replOutput @Text ":quit - give up and leave"

----------
