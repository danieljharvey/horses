{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions
  ( doReplAction,
    evaluateText,
    resolveStoreExpression,
  )
where

import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Language.Mimsa.Actions
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
import Language.Mimsa.Server.EnvVars
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project

doReplAction ::
  MimsaConfig ->
  Project Annotation ->
  Text ->
  ReplAction Annotation ->
  MimsaM (Error Annotation) (Project Annotation)
doReplAction mimsaConfig env input action =
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
    (Info expr) ->
      catchMimsaError env (doInfo env input expr $> env)
    (Bind name expr) ->
      catchMimsaError env (doBind env input name expr)
    (BindType dt) ->
      catchMimsaError env (doBindType env input dt)
    (OutputJS expr) ->
      catchMimsaError env (doOutputJS env input expr $> env)
    (TypeSearch mt) ->
      catchMimsaError env (doTypeSearch env mt $> env)
    (AddUnitTest testName testExpr) ->
      catchMimsaError env (doAddUnitTest env input testName testExpr)
    (ListTests maybeName) ->
      catchMimsaError env (doListTests env maybeName $> env)

----------

doHelp :: MimsaM e ()
doHelp = do
  logInfo "~~~ MIMSA ~~~"
  logInfo ":help - this help screen"
  logInfo ":info <expr> - get the type of <expr>"
  logInfo ":bind <name> = <expr> - binds <expr> to <name> and saves it in the environment"
  logInfo ":bindType type Either a b = Left a | Right b - binds a new type and saves it in the environment"
  logInfo ":list - show a list of current bindings in the environment"
  logInfo ":outputJS <expr> - show JS code for <expr>"
  logInfo ":tree <expr> - draw a dependency tree for <expr>"
  logInfo ":search <mt> - search for exprs that match type"
  logInfo ":addTest \"<test name>\" <expr> - add a unit test"
  logInfo ":listTests <optional name> - list tests for <name>"
  logInfo ":versions <name> - list all versions of a binding"
  logInfo "<expr> - Evaluate <expr>, returning it's simplified form and type"
  logInfo ":quit - give up and leave"

----------
