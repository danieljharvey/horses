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
import ReplNew.Actions.Compile
import ReplNew.Actions.Evaluate
import ReplNew.Actions.ExpressionBind
import ReplNew.Actions.ListModules
import ReplNew.Actions.Optimise
import ReplNew.Actions.Tree
import ReplNew.Actions.TypeSearch
import ReplNew.Actions.UnitTests
import ReplNew.Actions.Upgrade
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
    (Upgrade name) ->
      catchMimsaError env (doUpgrade env name $> env)
    (Optimise name) ->
      catchMimsaError env (doOptimise env name $> env)
    (Evaluate expr) ->
      catchMimsaError env (doEvaluate env input expr $> env)
    (Tree expr) ->
      catchMimsaError env (doTree env input expr $> env)
    (Graph expr) ->
      catchMimsaError env (doGraph env input expr $> env)
    ProjectGraph ->
      catchMimsaError env (doProjectGraph env $> env)
    (Bind name expr) ->
      catchMimsaError env (doBind env input name expr)
    (BindType dt) ->
      catchMimsaError env (doBindType env input dt)
    (OutputJS be expr) ->
      catchMimsaError env (doOutputJS env input be expr $> env)
    (TypeSearch mt) ->
      catchMimsaError env (doTypeSearch env mt $> env)
    (AddUnitTest testName testExpr) ->
      catchMimsaError env (doAddTest env input testName testExpr)
    (ListTests maybeName) ->
      catchMimsaError env (doListTests env maybeName $> env)

----------

doHelp :: ReplM e ()
doHelp = do
  replOutput @Text "~~~ MIMSA ~~~"
  replOutput @Text ":help - this help screen"
  replOutput @Text ":bind <name> = <expr> - binds <expr> to <name> and saves it in the environment"
  replOutput @Text ":bindType type Either a b = Left a | Right b - binds a new type and saves it in the environment"
  replOutput @Text ":modules - show a list of modules in the project"
  replOutput @Text ":outputJS <javascript|typescript> <expr> - save JS code for <expr>"
  replOutput @Text ":tree <expr> - draw a dependency tree for <expr>"
  replOutput @Text ":graph <expr> - output graphviz dependency tree for <expr>"
  replOutput @Text ":search <mt> - search for exprs that match type"
  replOutput @Text ":addTest \"<test name>\" <expr> - add a unit test"
  replOutput @Text ":tests <optional name> - list tests for <name>"
  replOutput @Text ":upgrade <name> - upgrade a binding to latest dependencies"
  replOutput @Text ":optimise <name> - run optimisations on latest version of dep"
  replOutput @Text "<expr> - Evaluate <expr>, returning it's simplified form and type"
  replOutput @Text ":quit - give up and leave"

----------
