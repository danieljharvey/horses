{-# LANGUAGE OverloadedStrings #-}

module Repl.Actions.Compile
  ( doOutputJS,
  )
where

import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Compile as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Backend.Backend
  ( copyLocalOutput,
  )
import Language.Mimsa.Backend.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Repl.Helpers
import Repl.ReplM
import Repl.Types

doOutputJS ::
  Project Annotation ->
  Text ->
  Maybe Backend ->
  Expr Name Annotation ->
  ReplM (Error Annotation) ()
doOutputJS project input maybeBackend expr = do
  let be = fromMaybe ESModulesJS maybeBackend
  (_, _, resolvedExpr) <-
    replMFromEither $
      Actions.run
        project
        (Actions.typecheckExpression project input expr)
  (_, (rootExprHash, exprHashes)) <-
    toReplM project (Actions.compile be (reStoreExpression resolvedExpr))
  outputPath <- doCopying be exprHashes rootExprHash
  replOutput ("Output to " <> outputPath)

doCopying ::
  Backend ->
  Set ExprHash ->
  ExprHash ->
  ReplM (Error Annotation) Text
doCopying be exprHashes rootExprHash = do
  rootPath <- asks rcRootPath
  mapError StoreErr (copyLocalOutput rootPath be exprHashes rootExprHash)
