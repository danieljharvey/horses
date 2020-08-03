{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions
  ( doReplAction,
    evaluateText,
    evaluateStoreExpression,
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
import Language.Mimsa.Project (getCurrentBindings)
import Language.Mimsa.Project.Versions
import Language.Mimsa.Repl.Types
import Language.Mimsa.Repl.Watcher
import Language.Mimsa.Store (createDepGraph, saveExpr)
import Language.Mimsa.Syntax (parseExpr)
import Language.Mimsa.Tui (goTui)
import Language.Mimsa.Types

doReplAction :: Project -> ReplAction -> IO Project
doReplAction env Help = do
  doHelp
  pure env
doReplAction env (ListBindings) = do
  _ <- runReplM $ doListBindings env
  pure env
doReplAction env Tui = do
  goTui env
doReplAction env (Versions name) = do
  _ <- runReplM $ doVersions env name
  pure env
doReplAction env Watch = do
  _ <- runReplM $ doWatch env
  pure env
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
  T.putStrLn "<expr> - Evaluate <expr>, returning it's simplified form and type"
  T.putStrLn ":quit - give up and leave"

----------

doBind :: Project -> Name -> Expr Name -> ReplM Project
doBind env name expr = do
  (type', storeExpr, _, _) <- liftRepl $ getTypecheckedStoreExpression env expr
  hash <- liftIO (saveExpr storeExpr)
  replPrint $
    "Bound " <> prettyPrint name <> " to " <> prettyPrint expr
      <> " :: "
      <> prettyPrint type'
  let newEnv = fromItem name storeExpr hash
  pure (env <> newEnv)

-------

doInfo :: Project -> Expr Name -> ReplM ()
doInfo env expr = do
  (type', _, _, _) <- liftRepl $ getTypecheckedStoreExpression env expr
  replPrint $
    prettyPrint expr
      <> " :: "
      <> prettyPrint type'

----------

doTree :: Project -> Expr Name -> ReplM ()
doTree env expr = do
  (_, storeExpr, _, _) <- liftRepl $ getTypecheckedStoreExpression env expr
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
  let showBind = \(name, (StoreExpression _ expr)) ->
        case getTypecheckedStoreExpression env expr of
          Right (type', _, _, _) ->
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
  (type', _, expr', scope') <- liftRepl $ getTypecheckedStoreExpression env expr
  simplified <- liftIO $ interpret scope' expr'
  simplified' <- liftRepl (first InterpreterErr simplified)
  replPrint $
    prettyPrint simplified'
      <> " :: "
      <> prettyPrint type'

---------

doWatch :: Project -> ReplM ()
doWatch env =
  liftIO $
    watchFile
      "./"
      ( do
          _ <- runReplM (onFileChange env)
          pure ()
      )

onFileChange :: Project -> ReplM ()
onFileChange env = do
  text <- liftIO $ T.readFile "./scratch.mimsa"
  replPrint ("scratch.mimsa updated!" :: Text)
  expr <- liftRepl $ first ParseErr (parseExpr (T.strip text))
  (type', storeExpr', expr', scope') <- liftRepl $ getTypecheckedStoreExpression env expr
  simplified <- liftIO $ interpret scope' expr'
  simplified' <- liftRepl (first InterpreterErr simplified)
  replPrint $
    "+ Using the following from scope: "
      <> prettyPrint (storeBindings storeExpr')
  replPrint $
    prettyPrint simplified'
      <> " :: "
      <> prettyPrint type'
