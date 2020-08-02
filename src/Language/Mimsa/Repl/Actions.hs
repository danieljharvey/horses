{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions
  ( doReplAction,
    evaluateText,
    evaluateStoreExpression,
  )
where

import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
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
  doListBindings env
  pure env
doReplAction env Tui = do
  goTui env
doReplAction env (Versions name) = do
  doVersions env name
  pure env
doReplAction env Watch = do
  doWatch env
  pure env
doReplAction env (Evaluate expr) = doEvaluate env expr
doReplAction env (Tree expr) = do
  _ <- runReplM $ doTree env expr
  pure env
doReplAction env (Info expr) = do
  doInfo env expr
  pure env
doReplAction env (Bind name expr) = doBind env name expr

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

doBind :: Project -> Name -> Expr Name -> IO Project
doBind env name expr = case getTypecheckedStoreExpression env expr of
  Left e' -> do
    T.putStrLn (prettyPrint e')
    pure env
  Right (type', storeExpr, _, _) -> do
    hash <- saveExpr storeExpr
    T.putStrLn $
      "Bound " <> prettyPrint name <> " to " <> prettyPrint expr
        <> " :: "
        <> prettyPrint type'
    let newEnv = fromItem name storeExpr hash
    pure (env <> newEnv)

-------

doInfo :: Project -> Expr Name -> IO ()
doInfo env expr =
  case getTypecheckedStoreExpression env expr of
    Left e' -> do
      T.putStrLn $ prettyPrint e'
    Right (type', _, _, _) -> do
      T.putStrLn $
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

doVersions :: Project -> Name -> IO ()
doVersions env name =
  case findVersions env name of
    Right versions ->
      let showIt (i, mt, expr', usages) = do
            T.putStrLn $ "#" <> T.pack (show i) <> (if NE.length versions == i then " (current)" else "")
            T.putStrLn $ (prettyPrint expr') <> ": " <> (prettyPrint mt)
            if S.null usages
              then T.putStrLn "Dependency of 0 functions"
              else
                T.putStrLn $
                  "Dependency of " <> (T.pack . show . S.size) usages
                    <> " functions"
       in traverse_ showIt versions
    Left e -> do
      T.putStrLn (prettyPrint e)

------

doListBindings :: Project -> IO ()
doListBindings env = do
  let showBind = \(name, (StoreExpression _ expr)) ->
        T.putStrLn $ case getTypecheckedStoreExpression env expr of
          Right (type', _, _, _) ->
            prettyPrint name <> " :: " <> prettyPrint type'
          _ -> ""
  traverse_ showBind (getExprPairs (store env) (getCurrentBindings $ bindings env))

----------

doEvaluate :: Project -> Expr Name -> IO Project
doEvaluate env expr = case getTypecheckedStoreExpression env expr of
  Left e' -> do
    T.putStrLn $ prettyPrint e'
    pure env
  Right (type', _, expr', scope') -> do
    simplified <- interpret scope' expr'
    case simplified of
      Left e -> do
        T.putStrLn (prettyPrint e)
        pure env
      Right simplified' -> do
        T.putStrLn $
          prettyPrint simplified'
            <> " :: "
            <> prettyPrint type'
        pure env

---------

doWatch :: Project -> IO ()
doWatch env =
  watchFile
    "./store/"
    ( do
        text <- T.readFile "./store/scratch.mimsa"
        T.putStrLn "scratch.mimsa updated!"
        case parseExpr (T.strip text) of
          Right expr -> do
            case getTypecheckedStoreExpression env expr of
              Left e' -> do
                T.putStrLn $ prettyPrint e'
              Right (type', storeExpr', expr', scope') -> do
                simplified <- interpret scope' expr'
                case simplified of
                  Left e -> do
                    T.putStrLn (prettyPrint e)
                  Right simplified' -> do
                    T.putStrLn $
                      "+ Using the following from scope: "
                        <> prettyPrint (storeBindings storeExpr')
                    T.putStrLn $
                      prettyPrint simplified'
                        <> " :: "
                        <> prettyPrint type'
          Left e -> do
            T.putStrLn e
    )
