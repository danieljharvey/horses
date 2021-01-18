{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.ExpressionBind
  ( doBind,
    doBindType,
    bindStoreExpression,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import Language.Mimsa.Actions
import Language.Mimsa.Printer
import Language.Mimsa.Repl.Types
import Language.Mimsa.Store (saveExpr)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

doBind ::
  Project Annotation ->
  Text ->
  Name ->
  Expr Name Annotation ->
  ReplM Annotation (Project Annotation)
doBind env input name expr = do
  (ResolvedExpression type' storeExpr _ _ _) <- liftRepl $ getTypecheckedStoreExpression input env expr
  replPrint $
    "Bound " <> prettyPrint name <> ".\n\n" <> prettyPrint expr
      <> "\n::\n"
      <> prettyPrint type'
  bindStoreExpression env storeExpr name

doBindType ::
  Project Annotation ->
  Text ->
  DataType ->
  ReplM Annotation (Project Annotation)
doBindType env input dt = do
  let expr = MyData mempty dt (MyRecord mempty mempty)
  (ResolvedExpression _ storeExpr _ _ _) <- liftRepl $ getTypecheckedStoreExpression input env expr
  replPrint $
    "Bound type " <> prettyPrint dt
  bindTypeExpression env storeExpr

bindTypeExpression ::
  Project ann ->
  StoreExpression ann ->
  ReplM ann (Project ann)
bindTypeExpression env storeExpr = do
  mimsaConfig <- ask
  hash <- lift $ withExceptT StoreErr $ saveExpr mimsaConfig storeExpr
  let newEnv = fromType storeExpr hash
  pure (env <> newEnv)

-- save an expression in the store and bind it to name
bindStoreExpression ::
  Project Annotation ->
  StoreExpression Annotation ->
  Name ->
  ReplM Annotation (Project Annotation)
bindStoreExpression env storeExpr name = do
  mimsaConfig <- ask
  hash <- lift $ withExceptT StoreErr $ saveExpr mimsaConfig storeExpr
  let newEnv = fromItem name storeExpr hash
  pure (env <> newEnv)
