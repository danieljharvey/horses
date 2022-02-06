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
  Backend ->
  Set ExprHash ->
  ExprHash ->
  MimsaM StoreError Text
copyLocalOutput be exprHashes rootExprHash = do
  modulePath <- createModuleOutputPath be
  indexPath <- createIndexOutputPath be
  stdlibPath <- createStdlibOutputPath be
  outputPath <- createOutputFolder be rootExprHash
  -- link modules
  traverse_ (copyModule modulePath outputPath be) exprHashes
  -- link stdlib
  _ <- copyStdlib stdlibPath outputPath be
  -- link index
  copyIndex indexPath outputPath be rootExprHash

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

-- the stdlib is already in the store so we copy it to the target folder
copyStdlib :: FilePath -> FilePath -> Backend -> MimsaM StoreError Text
copyStdlib stdlibPath outputPath be = do
  let fromPath = T.pack stdlibPath <> stdlibFilename be
  let toPath = T.pack outputPath <> stdlibFilename be
  tryCopy (T.unpack fromPath) (T.unpack toPath)
  pure toPath

-- the index is already in ths store so we copy it to the target folder
copyIndex ::
  FilePath ->
  FilePath ->
  Backend ->
  ExprHash ->
  MimsaM StoreError Text
copyIndex indexPath outputPath be rootExprHash = do
  let filename = T.unpack $ indexFilename be rootExprHash
      fromPath = indexPath <> filename
      toPath = outputPath <> filename
  tryCopy fromPath toPath
  pure (T.pack toPath)
