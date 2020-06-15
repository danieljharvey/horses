{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions
  ( doReplAction,
  )
where

import Control.Monad (join)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Debug.Trace
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Repl.Types
import Language.Mimsa.Store
  ( createStoreExpression,
    saveExpr,
    substitute,
  )
import Language.Mimsa.Syntax
import Language.Mimsa.Typechecker
import Language.Mimsa.Types

doReplAction :: StoreEnv -> ReplAction -> IO StoreEnv
doReplAction env Help = do
  T.putStrLn "~~~ MIMSA ~~~"
  T.putStrLn ":help - this help screen"
  T.putStrLn ":info <expr> - get the type of <expr>"
  T.putStrLn ":bind <name> = <expr> - binds <expr> to <name> and saves it in the environment"
  T.putStrLn ":list - show a list of current bindings in the environment"
  T.putStrLn ":quit - give up and leave"
  T.putStrLn "<expr> - Evaluate <expr>, returning it's simplified form and type"
  pure env
doReplAction env (ListBindings) = do
  let showBind = \(name, expr) -> T.putStrLn $ case getType env expr of
        Right type' ->
          prettyPrint name <> " :: " <> prettyPrint type'
        _ -> ""
  _ <- traverse showBind (getExprPairs (store env) (bindings env))
  pure env
doReplAction env (Evaluate expr) = do
  case getTypecheckedStoreExpression env expr >>= (\(type', _) -> (,) type' <$> interpret expr) of
    Left e' -> do
      print e'
      pure env
    Right (type', simplified) -> do
      T.putStrLn $
        prettyPrint simplified
          <> " :: "
          <> prettyPrint type'
      pure env
doReplAction env (Info expr) = do
  case getTypecheckedStoreExpression env expr of
    Left e' -> do
      print e'
      pure env
    Right (type', _) -> do
      T.putStrLn $
        prettyPrint expr
          <> " :: "
          <> prettyPrint type'
      pure env
doReplAction env (Bind name expr) = do
  if M.member name (getBindings $ bindings env)
    then do
      T.putStrLn $ T.pack (show name) <> " is already bound"
      pure env
    else do
      case getTypecheckedStoreExpression env expr of
        Left e' -> do
          print e'
          pure env
        Right (type', storeExpr) -> do
          hash <- saveExpr storeExpr
          T.putStrLn $
            "Bound " <> prettyPrint name <> " to " <> prettyPrint expr
              <> " :: "
              <> prettyPrint type'
          let newEnv = fromItem name storeExpr hash
          pure (env <> newEnv)

----------

getType :: StoreEnv -> StoreExpression -> Either T.Text MonoType
getType env storeExpr = do
  (_, expr, scope) <- substitute (store env) storeExpr
  startInference (chainExprs (traceShowId expr) (traceShowId scope))

getExprPairs :: Store -> Bindings -> [(Name, StoreExpression)]
getExprPairs (Store items') (Bindings bindings') = join $ do
  (name, hash) <- M.toList bindings'
  case M.lookup hash items' of
    Just item -> pure [(name, item)]
    _ -> pure []

chainExprs ::
  Expr ->
  Scope ->
  Expr
chainExprs expr scope =
  foldr (\(name, expr') a -> MyLet name expr' a) expr (M.toList . getScope $ scope)

fromItem :: Name -> StoreExpression -> ExprHash -> StoreEnv
fromItem name expr hash = StoreEnv
  { store = Store $ M.singleton hash expr,
    bindings = Bindings $ M.singleton name hash
  }

getTypecheckedStoreExpression :: StoreEnv -> Expr -> Either Text (MonoType, StoreExpression)
getTypecheckedStoreExpression env expr = do
  storeExpr <- createStoreExpression (bindings env) expr
  exprType <- getType (traceShowId env) (traceShowId storeExpr)
  pure (exprType, storeExpr)
