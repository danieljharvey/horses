{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.Compile
  ( doOutputJS,
  )
where

import Control.Monad.Reader
import Data.Set (Set)
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Compile as Actions
import Language.Mimsa.Backend.Backend
  ( Backend (..),
    copyLocalOutput,
  )
import Language.Mimsa.Repl.Helpers
import Language.Mimsa.Repl.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

doOutputJS ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  ReplM Annotation ()
doOutputJS project input expr = do
  (_, (rootExprHash, exprHashes)) <-
    toReplM project (Actions.compile CommonJS input expr)
  outputPath <- doCopying CommonJS exprHashes rootExprHash
  replPrint ("Output to " <> outputPath)

doCopying :: Backend -> Set ExprHash -> ExprHash -> ReplM Annotation Text
doCopying be exprHashes rootExprHash = do
  mimsaConfig <- ask
  liftExceptTToRepl StoreErr (copyLocalOutput mimsaConfig be exprHashes rootExprHash)
