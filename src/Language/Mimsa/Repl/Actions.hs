{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions
  ( doReplAction,
    evaluateText,
    resolveStoreExpression,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Actions
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Printer
import Language.Mimsa.Project (getCurrentBindings)
import Language.Mimsa.Project.Versions
import Language.Mimsa.Repl.ExpressionBind
import Language.Mimsa.Repl.ExpressionWatch
import Language.Mimsa.Repl.Types
import Language.Mimsa.Store (createDepGraph)
import Language.Mimsa.Tui (goTui)
import Language.Mimsa.Types

doReplAction :: Project -> ReplAction -> IO Project
doReplAction env Help = do
  doHelp
  pure env
doReplAction env ListBindings = do
  _ <- runReplM $ doListBindings env
  pure env
doReplAction env Tui =
  goTui env
doReplAction env (Versions name) = do
  _ <- runReplM $ doVersions env name
  pure env
doReplAction env (Watch name) = do
  newEnv' <- runReplM $ doWatch env name
  case newEnv' of
    Just env' -> pure env'
    _ -> pure env
doReplAction env (Evaluate expr) = do
  _ <- runReplM $ doEvaluate env expr
  pure env
doReplAction env (Tree expr) = do
  _ <- runReplM $ doTree env expr
  pure env
doReplAction env (Info expr) = do
  _ <- runReplM $ doInfo env expr
  pure env
doReplAction env (Bind name expr) = do
  newEnv <- runReplM $ doBind env name expr
  pure (fromMaybe env newEnv)
doReplAction env (BindType dt) = do
  newEnv <- runReplM $ doBindType env dt
  pure (fromMaybe env newEnv)

----------

doHelp :: IO ()
doHelp = do
  T.putStrLn "~~~ MIMSA ~~~"
  T.putStrLn ":help - this help screen"
  T.putStrLn ":info <expr> - get the type of <expr>"
  T.putStrLn ":bind <name> = <expr> - binds <expr> to <name> and saves it in the environment"
  T.putStrLn ":list - show a list of current bindings in the environment"
  T.putStrLn ":tree <expr> - draw a dependency tree for <expr>"
  T.putStrLn ":versions <name> - list all versions of a binding"
  T.putStrLn ":watch <name> - put <name> into 'scratch.mimsa' and bind any changes"
  T.putStrLn ":tui - launch terminal user interface for exploring project"
  T.putStrLn "<expr> - Evaluate <expr>, returning it's simplified form and type"
  T.putStrLn ":quit - give up and leave"

----------

doInfo :: Project -> Expr Name -> ReplM ()
doInfo env expr = do
  (ResolvedExpression type' _ _ _ _) <- liftRepl $ getTypecheckedStoreExpression env expr
  replPrint $
    prettyPrint expr
      <> " :: "
      <> prettyPrint type'

----------

doTree :: Project -> Expr Name -> ReplM ()
doTree env expr = do
  (ResolvedExpression _ storeExpr _ _ _) <- liftRepl $ getTypecheckedStoreExpression env expr
  let graph = createDepGraph (mkName "expression") (store env) storeExpr
  replPrint graph

-------

doVersions :: Project -> Name -> ReplM ()
doVersions env name = do
  versions <- liftRepl $ findVersions env name
  let showIt (i, mt, expr', usages) = do
        replPrint $
          "#" <> T.pack (show i)
            <> ( if NE.length versions == i
                   then " (current)"
                   else ""
               )
        replPrint (expr', mt)
        if S.null usages
          then replPrint ("Dependency of 0 functions" :: Text)
          else
            replPrint $
              "Dependency of " <> (T.pack . show . S.size) usages
                <> " functions"
   in traverse_ showIt versions

------

doListBindings :: Project -> ReplM ()
doListBindings env = do
  let showBind (name, StoreExpression expr _ _) =
        case getTypecheckedStoreExpression env expr of
          Right (ResolvedExpression type' _ _ _ _) ->
            replPrint (prettyPrint name <> " :: " <> prettyPrint type')
          _ -> pure ()
  traverse_
    showBind
    ( getExprPairs
        (store env)
        (getCurrentBindings $ bindings env)
    )

----------

doEvaluate :: Project -> Expr Name -> ReplM ()
doEvaluate env expr = do
  (ResolvedExpression type' _ expr' scope' swaps) <- liftRepl $ getTypecheckedStoreExpression env expr
  simplified <- liftIO $ interpret scope' swaps expr'
  simplified' <- liftRepl (first InterpreterErr simplified)
  replPrint $
    prettyPrint simplified'
      <> " :: "
      <> prettyPrint type'
---------
