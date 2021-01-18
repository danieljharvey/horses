{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions
  ( doReplAction,
    evaluateText,
    resolveStoreExpression,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Language.Mimsa.Actions
import Language.Mimsa.Repl.Actions.AddUnitTest
import Language.Mimsa.Repl.Actions.Compile
import Language.Mimsa.Repl.Actions.Evaluate
import Language.Mimsa.Repl.Actions.ExpressionBind
import Language.Mimsa.Repl.Actions.Info
import Language.Mimsa.Repl.Actions.ListBindings
import Language.Mimsa.Repl.Actions.Tree
import Language.Mimsa.Repl.Actions.TypeSearch
import Language.Mimsa.Repl.Actions.Versions (doVersions)
import Language.Mimsa.Repl.Types
import Language.Mimsa.Server.EnvVars
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project

doReplAction ::
  MimsaConfig ->
  Project Annotation ->
  Text ->
  ReplAction Annotation ->
  IO (Project Annotation)
doReplAction mimsaConfig env input action =
  case action of
    Help -> do
      doHelp
      pure env
    ListBindings -> do
      _ <- runReplM mimsaConfig $ doListBindings env input
      pure env
    (Versions name) -> do
      _ <- runReplM mimsaConfig $ doVersions env name
      pure env
    (Evaluate expr) -> do
      _ <- runReplM mimsaConfig $ doEvaluate env input expr
      pure env
    (Tree expr) -> do
      _ <- runReplM mimsaConfig $ doTree env input expr
      pure env
    (Info expr) -> do
      _ <- runReplM mimsaConfig $ doInfo env input expr
      pure env
    (Bind name expr) -> do
      newEnv <- runReplM mimsaConfig $ doBind env input name expr
      pure (fromMaybe env newEnv)
    (BindType dt) -> do
      newEnv <- runReplM mimsaConfig $ doBindType env input dt
      pure (fromMaybe env newEnv)
    (OutputJS expr) -> do
      _ <- runReplM mimsaConfig (doOutputJS env input expr)
      pure env
    (TypeSearch mt) ->
      runReplM mimsaConfig (doTypeSearch env mt) >> pure env
    (AddUnitTest testName testExpr) -> do
      newEnv <- runReplM mimsaConfig (doAddUnitTest env input testName testExpr)
      pure (fromMaybe env newEnv)

----------

doHelp :: IO ()
doHelp = do
  T.putStrLn "~~~ MIMSA ~~~"
  T.putStrLn ":help - this help screen"
  T.putStrLn ":info <expr> - get the type of <expr>"
  T.putStrLn ":bind <name> = <expr> - binds <expr> to <name> and saves it in the environment"
  T.putStrLn ":bindType type Either a b = Left a | Right b - binds a new type and saves it in the environment"
  T.putStrLn ":list - show a list of current bindings in the environment"
  T.putStrLn ":outputJS <expr> - show JS code for <expr>"
  T.putStrLn ":tree <expr> - draw a dependency tree for <expr>"
  T.putStrLn ":search <mt> - search for exprs that match type"
  T.putStrLn ":addUnitTest \"<test name>\" <expr> - add a unit test"
  T.putStrLn ":versions <name> - list all versions of a binding"
  T.putStrLn "<expr> - Evaluate <expr>, returning it's simplified form and type"
  T.putStrLn ":quit - give up and leave"

----------
