{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.BindType
  ( bindType,
  )
where

import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
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

addTypeToProject ::
  Text ->
  DataType ->
  Actions.ActionM (StoreExpression Annotation)
addTypeToProject _input dt = do
  -- create storeExpr for new datatype
  let se = StoreDataType dt mempty
  Actions.bindTypeExpression se
  pure se
