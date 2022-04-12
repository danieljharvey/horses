{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Backend.Backend
  ( outputStoreExpression,
    copyLocalOutput,
    Backend (..),
  )
where

import Control.Monad.Except
import Control.Monad.Logger
import Data.Foldable (traverse_)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Output
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Store.Storage (tryCopy)
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Store.RootPath

-- given output type and list of expressions, copy everything to local
-- folder for output in repl
copyLocalOutput ::
  (MonadIO m, MonadLogger m) =>
  RootPath ->
  Backend ->
  Set ExprHash ->
  ExprHash ->
  m Text
copyLocalOutput rootPath be exprHashes rootExprHash = do
  modulePath <- createModuleOutputPath rootPath be
  indexPath <- createIndexOutputPath rootPath be
  stdlibPath <- createStdlibOutputPath rootPath be
  outputPath <- createOutputFolder be rootExprHash
  -- link modules
  traverse_ (copyModule modulePath outputPath be) exprHashes
  -- link stdlib
  _ <- copyStdlib stdlibPath outputPath be
  -- link index
  copyIndex indexPath outputPath be rootExprHash

copyModule ::
  (MonadIO m, MonadLogger m) =>
  FilePath ->
  FilePath ->
  Backend ->
  ExprHash ->
  m ()
copyModule modulePath outputPath be exprHash = do
  let filename = moduleFilename be exprHash <> fileExtension be
      fromPath = modulePath <> T.unpack filename
      toPath = outputPath <> T.unpack filename
  tryCopy fromPath toPath

-- the stdlib is already in the store so we copy it to the target folder
copyStdlib :: (MonadIO m, MonadLogger m) => FilePath -> FilePath -> Backend -> m Text
copyStdlib stdlibPath outputPath be = do
  let fromPath = T.pack stdlibPath <> stdlibFilename be
  let toPath = T.pack outputPath <> stdlibFilename be
  tryCopy (T.unpack fromPath) (T.unpack toPath)
  pure toPath

-- the index is already in ths store so we copy it to the target folder
copyIndex ::
  (MonadIO m, MonadLogger m) =>
  FilePath ->
  FilePath ->
  Backend ->
  ExprHash ->
  m Text
copyIndex indexPath outputPath be rootExprHash = do
  let filename = T.unpack $ indexFilename be rootExprHash
      fromPath = indexPath <> filename
      toPath = outputPath <> filename
  tryCopy fromPath toPath
  pure (T.pack toPath)
