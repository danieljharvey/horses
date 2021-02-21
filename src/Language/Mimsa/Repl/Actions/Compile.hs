{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.Compile
  ( doOutputJS,
  )
where

import Data.Set (Set)
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Compile as Actions
import Language.Mimsa.Backend.Backend
  ( Backend (..),
    copyLocalOutput,
  )
import Language.Mimsa.Monad
import Language.Mimsa.Repl.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

doOutputJS ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  MimsaM (Error Annotation) ()
doOutputJS project input expr = do
  (_, (rootExprHash, exprHashes)) <-
    toReplM project (Actions.compile CommonJS input expr)
  outputPath <- doCopying CommonJS exprHashes rootExprHash
  logInfo ("Output to " <> outputPath)

doCopying ::
  Backend ->
  Set ExprHash ->
  ExprHash ->
  MimsaM (Error Annotation) Text
doCopying be exprHashes rootExprHash =
  mapError StoreErr (copyLocalOutput be exprHashes rootExprHash)
