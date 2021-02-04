{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.BindType
  ( bindType,
  )
where

import Control.Monad.Except (liftEither)
import Data.Text (Text)
import Language.Mimsa.Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Typechecker.Codegen
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression

-- add a new type to the project, creating any functions for it as we go
bindType ::
  Text ->
  DataType ->
  Name ->
  Actions.ActionM [Typeclass]
bindType input dt name = do
  project <- Actions.getProject
  let expr = MyData mempty dt (MyRecord mempty mempty)
  (ResolvedExpression _type' storeExpr _ _ _) <-
    liftEither $ getTypecheckedStoreExpression input project expr
  Actions.bindStoreExpression storeExpr name
  case lookupBindingName project name of
    Nothing -> do
      Actions.appendMessage
        ( "Bound type " <> prettyPrint name <> "."
        )
      pure mempty
    Just _oldExprHash ->
      do
        Actions.appendMessage
          ( "Updated type binding of " <> prettyPrint name <> "."
          )
        pure mempty
