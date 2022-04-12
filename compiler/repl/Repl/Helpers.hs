module Repl.Helpers
  ( saveExpression,
    toReplM,
    catchMimsaError,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable (traverse_)
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Repl.ReplM
import Repl.Types

-- | if an error has been thrown, log it and return default value
catchMimsaError ::
  a ->
  ReplM e a ->
  ReplM e a
catchMimsaError def computation =
  computation `catchError` \_e -> do
    pure def

-- | Actually save a StoreExpression to disk
saveExpression ::
  StoreExpression Annotation ->
  ReplM (Error Annotation) ExprHash
saveExpression se = do
  rootPath <- asks rcRootPath
  mapError StoreErr (saveExpr rootPath se)

-- | Actually save a file to disk
saveFile' ::
  (Actions.SavePath, Actions.SaveFilename, Actions.SaveContents) ->
  ReplM (Error Annotation) ()
saveFile' details = do
  rootPath <- asks rcRootPath
  mapError StoreErr (saveFile rootPath details)

-- | Run an Action, printing any messages to the console and saving any
-- expressions to disk
toReplM ::
  Project Annotation ->
  Actions.ActionM a ->
  ReplM (Error Annotation) (Project Annotation, a)
toReplM project action = case Actions.run project action of
  Left e -> do
    replOutput e
    throwError e
  Right (newProject, outcomes, a) -> do
    traverse_ replOutput (Actions.messagesFromOutcomes outcomes)
    traverse_ saveExpression (Actions.storeExpressionsFromOutcomes outcomes)
    traverse_ saveFile' (Actions.writeFilesFromOutcomes outcomes)
    pure (newProject, a)
