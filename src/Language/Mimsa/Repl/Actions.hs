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
import Language.Mimsa.Repl.Compile
import Language.Mimsa.Repl.Evaluate
import Language.Mimsa.Repl.ExpressionBind
import Language.Mimsa.Repl.Info
import Language.Mimsa.Repl.ListBindings
import Language.Mimsa.Repl.Tree
import Language.Mimsa.Repl.TypeSearch
import Language.Mimsa.Repl.Types
import Language.Mimsa.Repl.Versions (doVersions)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project

doReplAction ::
  Project Annotation ->
  Text ->
  ReplAction Annotation ->
  IO (Project Annotation)
doReplAction env _ Help = do
  doHelp
  pure env
doReplAction env input ListBindings =
  runReplM (doListBindings env input) >> pure env
doReplAction env _ (Versions name) =
  runReplM (doVersions env name) >> pure env
doReplAction env input (Evaluate expr) =
  runReplM (doEvaluate env input expr) >> pure env
doReplAction env input (Tree expr) =
  runReplM (doTree env input expr) >> pure env
doReplAction env input (Info expr) =
  runReplM (doInfo env input expr) >> pure env
doReplAction env input (Bind name expr) = do
  newEnv <- runReplM $ doBind env input name expr
  pure (fromMaybe env newEnv)
doReplAction env input (BindType dt) = do
  newEnv <- runReplM $ doBindType env input dt
  pure (fromMaybe env newEnv)
doReplAction env input (OutputJS expr) =
  runReplM (doOutputJS env input expr) >> pure env
doReplAction env _input (TypeSearch mt) =
  runReplM (doTypeSearch env mt) >> pure env

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
  T.putStrLn ":versions <name> - list all versions of a binding"
  T.putStrLn "<expr> - Evaluate <expr>, returning it's simplified form and type"
  T.putStrLn ":quit - give up and leave"
