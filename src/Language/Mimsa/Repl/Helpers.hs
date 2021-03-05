{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Helpers
  ( saveExpression,
    toReplM,
    catchMimsaError,
  )
where

import Control.Monad.Except
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Monad
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

-- | if an error has been thrown, log it and return default value
catchMimsaError ::
  (Printer e) =>
  a ->
  MimsaM e a ->
  MimsaM e a
catchMimsaError def computation =
  computation `catchError` \e -> do
    logDebug (prettyPrint e)
    pure def

-- | Actually save a StoreExpression to disk
saveExpression ::
  StoreExpression Annotation ->
  MimsaM (Error Annotation) ExprHash
saveExpression =
  mapError StoreErr . saveExpr

-- | given an expression to save, save it
saveFile ::
  (Actions.SavePath, Actions.SaveFilename, Actions.SaveContents) ->
  MimsaM (Error Annotation) ()
saveFile (path, filename, content) = do
  fullPath <- getStoreFolder (show path)
  let savePath = fullPath <> show filename
  logDebug $ "Saving to " <> T.pack savePath
  liftIO $ LBS.writeFile savePath (coerce content)

-- | Run an Action, printing any messages to the console and saving any
-- expressions to disk
toReplM ::
  Project Annotation ->
  Actions.ActionM a ->
  MimsaM (Error Annotation) (Project Annotation, a)
toReplM project action = case Actions.run project action of
  Left e -> throwError e
  Right (newProject, outcomes, a) -> do
    traverse_ replOutput (Actions.messagesFromOutcomes outcomes)
    traverse_ saveExpression (Actions.storeExpressionsFromOutcomes outcomes)
    traverse_ saveFile (Actions.writeFilesFromOutcomes outcomes)
    pure (newProject, a)
