{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.ExpressionBind
  ( doBind,
    doBindType,
    bindStoreExpression,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import Language.Mimsa.Actions
import Language.Mimsa.Printer
import Language.Mimsa.Repl.Types
import Language.Mimsa.Store (saveExpr)
import Language.Mimsa.Types

doBind ::
  (Eq ann, Monoid ann, JSON.ToJSON ann) =>
  Project ann ->
  Name ->
  Expr ann Name ->
  ReplM ann (Project ann)
doBind env name expr = do
  (ResolvedExpression type' storeExpr _ _ _) <- liftRepl $ getTypecheckedStoreExpression env expr
  replPrint $
    "Bound " <> prettyPrint name <> " to " <> prettyPrint expr
      <> " :: "
      <> prettyPrint type'
  bindStoreExpression env storeExpr name

doBindType ::
  (Eq ann, Monoid ann, JSON.ToJSON ann) =>
  Project ann ->
  DataType ->
  ReplM ann (Project ann)
doBindType env dt = do
  let expr = MyData mempty dt (MyRecord mempty mempty)
  (ResolvedExpression _ storeExpr _ _ _) <- liftRepl $ getTypecheckedStoreExpression env expr
  replPrint $
    "Bound type " <> prettyPrint dt
  bindTypeExpression env storeExpr

bindTypeExpression ::
  (MonadIO m, JSON.ToJSON ann) =>
  Project ann ->
  StoreExpression ann ->
  m (Project ann)
bindTypeExpression env storeExpr = do
  hash <- liftIO (saveExpr storeExpr)
  let newEnv = fromType storeExpr hash
  pure (env <> newEnv)

-- save an expression in the store and bind it to name
bindStoreExpression ::
  (MonadIO m, JSON.ToJSON ann) =>
  Project ann ->
  StoreExpression ann ->
  Name ->
  m (Project ann)
bindStoreExpression env storeExpr name = do
  hash <- liftIO (saveExpr storeExpr)
  let newEnv = fromItem name storeExpr hash
  pure (env <> newEnv)
