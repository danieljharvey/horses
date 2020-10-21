{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.ExpressionBind
  ( doBind,
    doBindType,
    bindStoreExpression,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Language.Mimsa.Actions
import Language.Mimsa.Printer
import Language.Mimsa.Repl.Types
import Language.Mimsa.Store (saveExpr)
import Language.Mimsa.Types

doBind ::
  Project Annotation ->
  Name ->
  Expr Name Annotation ->
  ReplM Annotation (Project Annotation)
doBind env name expr = do
  (ResolvedExpression type' storeExpr _ _ _) <- liftRepl $ getTypecheckedStoreExpression env expr
  replPrint $
    "Bound " <> prettyPrint name <> ".\n\n" <> prettyPrint expr
      <> "\n::\n"
      <> prettyPrint type'
  bindStoreExpression env storeExpr name

doBindType ::
  Project Annotation ->
  DataType ->
  ReplM Annotation (Project Annotation)
doBindType env dt = do
  let expr = MyData mempty dt (MyRecord mempty mempty)
  (ResolvedExpression _ storeExpr _ _ _) <- liftRepl $ getTypecheckedStoreExpression env expr
  replPrint $
    "Bound type " <> prettyPrint dt
  bindTypeExpression env storeExpr

bindTypeExpression ::
  (MonadIO m) =>
  Project ann ->
  StoreExpression ann ->
  m (Project ann)
bindTypeExpression env storeExpr = do
  hash <- liftIO (saveExpr storeExpr)
  let newEnv = fromType storeExpr hash
  pure (env <> newEnv)

-- save an expression in the store and bind it to name
bindStoreExpression ::
  (MonadIO m) =>
  Project Annotation ->
  StoreExpression Annotation ->
  Name ->
  m (Project Annotation)
bindStoreExpression env storeExpr name = do
  hash <- liftIO (saveExpr storeExpr)
  let newEnv = fromItem name storeExpr hash
  pure (env <> newEnv)
