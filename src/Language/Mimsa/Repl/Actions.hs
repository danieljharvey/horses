{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions
  ( doReplAction,
    evaluateText,
    evaluateStoreExpression,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Actions
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Repl.Types
import Language.Mimsa.Repl.Watcher
import Language.Mimsa.Store
  ( createDepGraph,
    getCurrentBindings,
    saveExpr,
  )
import Language.Mimsa.Syntax (parseExpr)
import Language.Mimsa.Tui (goTui)
import Language.Mimsa.Types

doReplAction :: Project -> ReplAction -> IO Project
doReplAction env Help = do
  T.putStrLn "~~~ MIMSA ~~~"
  T.putStrLn ":help - this help screen"
  T.putStrLn ":info <expr> - get the type of <expr>"
  T.putStrLn ":bind <name> = <expr> - binds <expr> to <name> and saves it in the environment"
  T.putStrLn ":list - show a list of current bindings in the environment"
  T.putStrLn ":tree <expr> - draw a dependency tree for <expr>"
  T.putStrLn "<expr> - Evaluate <expr>, returning it's simplified form and type"
  T.putStrLn ":quit - give up and leave"
  pure env
doReplAction env (ListBindings) = do
  let showBind = \(name, (StoreExpression _ expr)) -> T.putStrLn $ case getTypecheckedStoreExpression env expr of
        Right (type', _, _, _) ->
          prettyPrint name <> " :: " <> prettyPrint type'
        _ -> ""
  _ <- traverse showBind (getExprPairs (store env) (getCurrentBindings $ bindings env))
  pure env
doReplAction env Tui = do
  goTui env
doReplAction env Watch = do
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
  pure env
doReplAction env (Evaluate expr) = do
  case getTypecheckedStoreExpression env expr of
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
doReplAction env (Tree expr) = do
  case getTypecheckedStoreExpression env expr of
    Left e' -> do
      T.putStrLn (prettyPrint e')
      pure env
    Right (_, storeExpr, _, _) -> do
      let graph = createDepGraph (mkName "expression") (store env) storeExpr
      T.putStrLn (prettyPrint graph)
      pure env
doReplAction env (Info expr) = do
  case getTypecheckedStoreExpression env expr of
    Left e' -> do
      T.putStrLn $ prettyPrint e'
      pure env
    Right (type', _, _, _) -> do
      T.putStrLn $
        prettyPrint expr
          <> " :: "
          <> prettyPrint type'
      pure env
doReplAction env (Bind name expr) = do
  case getTypecheckedStoreExpression env expr of
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
----------
