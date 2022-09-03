{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.BindType
  ( bindType,
  )
where

import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

-- add a new type to the project, creating any functions for it as we go
bindType ::
  Text ->
  DataType ->
  Actions.ActionM DataType
bindType input dt = do
  _ <- addTypeToProject input dt
  Actions.appendMessage
    ( "Bound type " <> prettyPrint (dtName dt) <> "."
    )
  pure dt

storeExprFromResolved :: ResolvedExpression ann -> StoreExpression ann
storeExprFromResolved = reStoreExpression

addTypeToProject ::
  Text ->
  DataType ->
  Actions.ActionM (StoreExpression Annotation)
addTypeToProject input dt = do
  project <- Actions.getProject
  -- create storeExpr for new datatype
  resolvedTypeExpr <-
    Actions.typecheckExpression
      project
      input
      ( MyData
          mempty
          dt
          (MyRecord mempty mempty)
      )
  Actions.bindTypeExpression (reStoreExpression resolvedTypeExpr)
  pure (storeExprFromResolved resolvedTypeExpr)
