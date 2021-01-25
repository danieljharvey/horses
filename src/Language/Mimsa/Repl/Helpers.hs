{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Helpers (saveExpression, toReplM) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable (traverse_)
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Repl.Types
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

-- | Actually save a StoreExpression to disk
saveExpression ::
  StoreExpression Annotation ->
  ReplM Annotation ExprHash
saveExpression storeExpr = do
  mimsaConfig <- ask
  lift $ withExceptT StoreErr $ saveExpr mimsaConfig storeExpr

-- | Run an Action, printing any messages to the console and saving any
-- expressions to disk
toReplM ::
  Project Annotation ->
  Actions.ActionM a ->
  ReplM Annotation (Project Annotation, a)
toReplM project action = case Actions.run project action of
  Left e -> throwError e
  Right (newProject, outcomes, a) -> do
    traverse_ replPrint (Actions.messagesFromOutcomes outcomes)
    traverse_ saveExpression (Actions.storeExpressionsFromOutcomes outcomes)
    pure (newProject, a)
