{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Backend
  ( outputCommonJS,
    getStdlib,
    copyLocalOutput,
    Backend (..),
  )
where

import Control.Monad.Except
import Data.Coerce
import Data.Foldable (traverse_)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Monad
import Language.Mimsa.Store.Storage (getStoreFolder, tryCopy)
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Store
import System.Directory

------

-- each expression is symlinked from the store to ./output/<exprhash>/<filename.ext>
createOutputFolder :: Backend -> ExprHash -> MimsaM e FilePath
createOutputFolder CommonJS exprHash = do
  let outputPath = symlinkedOutputPath CommonJS
  let path = outputPath <> show exprHash
  liftIO $ createDirectoryIfMissing True path
  pure (path <> "/")

-- all files are created in the store and then symlinked into output folders
-- this creates the folder in the store
createModuleOutputPath :: Backend -> MimsaM e FilePath
createModuleOutputPath be =
  getStoreFolder (transpiledModuleOutputPath be)

-- all files are created in the store and then symlinked into output folders
-- this creates the folder in the store
createIndexOutputPath :: Backend -> MimsaM e FilePath
createIndexOutputPath be =
  getStoreFolder (transpiledIndexOutputPath be)

-- all files are created in the store and then symlinked into output folders
-- this creates the folder in the store
createStdlibOutputPath :: Backend -> MimsaM e FilePath
createStdlibOutputPath be =
  getStoreFolder (transpiledStdlibOutputPath be)

getStdlib :: Backend -> Text
getStdlib CommonJS = coerce commonJSStandardLibrary

-- given output type and list of expressions, copy everything to local
-- folder for output in repl
copyLocalOutput ::
  Backend ->
  Set ExprHash ->
  ExprHash ->
  MimsaM StoreError Text
copyLocalOutput be exprHashes rootExprHash = do
  modulePath <- createModuleOutputPath be
  stdlibPath <- createStdlibOutputPath be
  indexPath <- createIndexOutputPath be
  outputPath <- createOutputFolder be rootExprHash
  -- link modules
  traverse_ (copyModule modulePath outputPath be) exprHashes
  -- link stdlib
  _ <- copyStdlib stdlibPath outputPath be
  -- link index
  copyIndex indexPath outputPath be

copyModule :: FilePath -> FilePath -> Backend -> ExprHash -> MimsaM StoreError ()
copyModule modulePath outputPath be exprHash = do
  let filename = moduleFilename be exprHash
      fromPath = modulePath <> T.unpack filename
      toPath = outputPath <> T.unpack filename
  tryCopy fromPath toPath

-- the stdlib is already in the store so we copy it to the target folder
copyStdlib :: FilePath -> FilePath -> Backend -> MimsaM StoreError Text
copyStdlib stdlibPath outputPath be = do
  let fromPath = T.pack stdlibPath <> stdLibFilename be
  let toPath = T.pack outputPath <> stdLibFilename be
  tryCopy (T.unpack fromPath) (T.unpack toPath)
  pure toPath

-- the index is already in ths store so we copy it to the target folder
copyIndex :: FilePath -> FilePath -> Backend -> MimsaM StoreError Text
copyIndex indexPath outputPath be = do
  let filename = T.unpack $ indexFilename be
      fromPath = indexPath <> filename
      toPath = outputPath <> filename
  tryCopy fromPath toPath
  pure (T.pack toPath)
