{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.Compile
  ( doOutputJS,
  )
where

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Backend.Backend
  ( copyLocalOutput,
  )
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Backend.Types
import Language.Mimsa.Monad
import Language.Mimsa.Repl.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

doOutputJS ::
  Project Annotation ->
  Text ->
  Maybe Backend ->
  Expr Name Annotation ->
  MimsaM (Error Annotation) ()
doOutputJS project input be expr = do
  let runtime = case fromMaybe ESModulesJS be of
        ESModulesJS -> ejsExportRuntime
        Typescript -> tsExportRuntime
  resolvedExpr <-
    mimsaFromEither $ Actions.getTypecheckedStoreExpression input project expr
  (_, (rootExprHash, exprHashes)) <-
    toReplM project (Actions.compile runtime input (reStoreExpression resolvedExpr))
  outputPath <- doCopying runtime exprHashes rootExprHash
  replOutput ("Output to " <> outputPath)

doCopying ::
  Runtime code ->
  Set ExprHash ->
  ExprHash ->
  MimsaM (Error Annotation) Text
doCopying runtime exprHashes rootExprHash =
  mapError StoreErr (copyLocalOutput runtime exprHashes rootExprHash)
