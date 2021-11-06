{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Backend
  ( outputStoreExpression,
    copyLocalOutput,
    Backend (..),
  )
where

import Data.Foldable (traverse_)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Output
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Monad
import Language.Mimsa.Store.Storage (tryCopy)
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Store

-- given output type and list of expressions, copy everything to local
-- folder for output in repl
copyLocalOutput ::
  Runtime code ->
  Set ExprHash ->
  ExprHash ->
  MimsaM StoreError Text
copyLocalOutput runtime exprHashes rootExprHash = do
  modulePath <- createModuleOutputPath (rtBackend runtime)
  indexPath <- createIndexOutputPath (rtBackend runtime)
  outputPath <- createOutputFolder (rtBackend runtime) rootExprHash
  -- link modules
  traverse_ (copyModule modulePath outputPath (rtBackend runtime)) exprHashes
  -- link index
  copyIndex indexPath outputPath runtime rootExprHash

copyModule ::
  FilePath ->
  FilePath ->
  Backend ->
  ExprHash ->
  MimsaM StoreError ()
copyModule modulePath outputPath be exprHash = do
  let filename = moduleFilename be exprHash <> fileExtension be
      fromPath = modulePath <> T.unpack filename
      toPath = outputPath <> T.unpack filename
  tryCopy fromPath toPath

-- the index is already in ths store so we copy it to the target folder
copyIndex ::
  FilePath ->
  FilePath ->
  Runtime code ->
  ExprHash ->
  MimsaM StoreError Text
copyIndex indexPath outputPath runtime rootExprHash = do
  let filename = T.unpack $ indexFilename runtime rootExprHash
      fromPath = indexPath <> filename
      toPath = outputPath <> filename
  tryCopy fromPath toPath
  pure (T.pack toPath)
