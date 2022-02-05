{-# LANGUAGE OverloadedStrings #-}

module Repl.Actions.Compile
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
import Language.Mimsa.Backend.Types
import Language.Mimsa.Monad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Repl.Helpers

doOutputJS ::
  Project Annotation ->
  Text ->
  Maybe Backend ->
  Expr Name Annotation ->
  MimsaM (Error Annotation) ()
doOutputJS project input maybeBackend expr = do
  let be = fromMaybe ESModulesJS maybeBackend
  resolvedExpr <-
    mimsaFromEither $ Actions.getTypecheckedStoreExpression input project expr
  (_, (rootExprHash, exprHashes)) <-
    toReplM project (Actions.compile be (reStoreExpression resolvedExpr))
  outputPath <- doCopying be exprHashes rootExprHash
  replOutput ("Output to " <> outputPath)

doCopying ::
  Backend ->
  Set ExprHash ->
  ExprHash ->
  MimsaM (Error Annotation) Text
doCopying be exprHashes rootExprHash =
  mapError StoreErr (copyLocalOutput be exprHashes rootExprHash)
