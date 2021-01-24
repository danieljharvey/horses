{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Helpers where

import Control.Monad.Except
import Data.Foldable (traverse_)
import Data.Set (Set)
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Repl.Actions.Shared
import Language.Mimsa.Repl.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

toReplM ::
  Project Annotation ->
  Actions.ActionM a ->
  ReplM Annotation (Project Annotation, Set (StoreExpression Annotation), [Text], a)
toReplM project action = case Actions.run project action of
  Left e -> throwError e
  Right (Actions.ActionState newProject storeExprs messages, a) -> do
    traverse_ replPrint messages
    traverse_ saveExpression storeExprs
    pure (newProject, storeExprs, messages, a)
