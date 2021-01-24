{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.ExpressionBind
  ( doBind,
    doBindType,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable
import Data.Text (Text)
import Language.Mimsa.Actions
import Language.Mimsa.Actions.BindExpression
import Language.Mimsa.Printer
import Language.Mimsa.Project.UnitTest
import Language.Mimsa.Repl.Helpers
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
  (newProject, _, _, (newExprHash, _)) <-
    toReplM project (bindExpression expr name input)
  traverse_
    (replPrint . prettyPrint)
    (getTestsForExprHash newProject newExprHash)
  pure newProject

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
