{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.ExpressionBind
  ( doBind,
    doBindType,
    bindStoreExpression,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable
import Data.Text (Text)
import Language.Mimsa.Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers (lookupBindingName)
import Language.Mimsa.Project.UnitTest
import Language.Mimsa.Repl.Actions.Shared
import Language.Mimsa.Repl.Types
import Language.Mimsa.Store.Storage
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
doBind project input name expr = do
  (ResolvedExpression type' storeExpr _ _ _) <- liftRepl $ getTypecheckedStoreExpression input project expr
  newProject <- bindStoreExpression project storeExpr name
  case lookupBindingName project name of
    Nothing -> do
      replPrint $
        "Bound " <> prettyPrint name <> ".\n" <> prettyPrint expr
          <> "\n::\n"
          <> prettyPrint type'
      pure newProject
    Just oldExprHash -> do
      replPrint $
        "Updated binding of " <> prettyPrint name <> ".\n" <> prettyPrint expr
          <> "\n::\n"
          <> prettyPrint type'
      let newExprHash = getStoreExpressionHash storeExpr
      (projectWithNewTests, newExprs) <-
        liftRepl $
          createNewUnitTests newProject oldExprHash newExprHash
      traverse_ saveExpression newExprs
      traverse_
        (replPrint . prettyPrint)
        (getTestsForExprHash projectWithNewTests newExprHash)
      pure projectWithNewTests

doBindType ::
  Project Annotation ->
  Text ->
  DataType ->
  ReplM Annotation (Project Annotation)
doBindType project input dt = do
  let expr = MyData mempty dt (MyRecord mempty mempty)
  (ResolvedExpression _ storeExpr _ _ _) <-
    liftRepl $ getTypecheckedStoreExpression input project expr
  replPrint $
    "Bound type " <> prettyPrint dt
  bindTypeExpression project storeExpr

bindTypeExpression ::
  Project ann ->
  StoreExpression ann ->
  ReplM ann (Project ann)
bindTypeExpression project storeExpr = do
  mimsaConfig <- ask
  hash <- lift $ withExceptT StoreErr $ saveExpr mimsaConfig storeExpr
  let newProject = fromType storeExpr hash
  pure (project <> newProject)

-- save an expression in the store and bind it to name
bindStoreExpression ::
  Project Annotation ->
  StoreExpression Annotation ->
  Name ->
  ReplM Annotation (Project Annotation)
bindStoreExpression project storeExpr name = do
  hash <- saveExpression storeExpr
  let newProject = fromItem name storeExpr hash
  pure (project <> newProject)
