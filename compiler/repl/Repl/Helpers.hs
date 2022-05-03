{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Repl.Helpers
  ( saveExpression,
    toReplM,
    catchMimsaError,
    outputErrorAsDiagnostic,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Error.Diagnose
import Error.Diagnose.Compat.Megaparsec
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Error.TypeError
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Repl.ReplM
import Repl.Types

instance HasHints Void msg where
  hints _ = mempty

-- | if an error has been thrown, log it and return default value
catchMimsaError ::
  a ->
  ReplM e a ->
  ReplM e a
catchMimsaError defValue computation =
  computation `catchError` \_e -> do
    pure defValue

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
    outputErrorAsDiagnostic e
    throwError e
  Right (newProject, outcomes, a) -> do
    traverse_ replOutput (Actions.messagesFromOutcomes outcomes)
    traverse_ saveExpression (Actions.storeExpressionsFromOutcomes outcomes)
    traverse_ saveFile' (Actions.writeFilesFromOutcomes outcomes)
    pure (newProject, a)

-- use diagnostics for errors where possible, falling back to boring errors
outputErrorAsDiagnostic :: Error Annotation -> ReplM e ()
outputErrorAsDiagnostic err' =
  let diag = errorToDiagnostic err'
   in printDiagnostic stderr True True 4 diag

errorToDiagnostic :: Error Annotation -> Diagnostic Text
errorToDiagnostic (ParseError input bundle) =
  let filename = "<repl>"
      diag = errorDiagnosticFromBundle Nothing "Parse error on input" Nothing bundle
   in --   Creates a new diagnostic with no default hints from the bundle returned by megaparsec
      addFile diag filename (T.unpack input)
errorToDiagnostic (TypeErr input typeErr) =
  typeErrorDiagnostic input typeErr
errorToDiagnostic e =
  let report =
        err
          Nothing
          (prettyPrint e)
          []
          []
   in addReport def report
