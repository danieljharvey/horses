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
import Language.Mimsa.Server.EnvVars
import Language.Mimsa.Store.Storage (getStoreFolder, tryCopy)
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Store
import System.Directory

------

-- each expression is symlinked from the store to ./output/<exprhash>/<filename.ext>
createOutputFolder :: Backend -> ExprHash -> IO FilePath
createOutputFolder CommonJS exprHash = do
  let outputPath = symlinkedOutputPath CommonJS
  let path = outputPath <> show exprHash
  createDirectoryIfMissing True path
  pure (path <> "/")

-- all files are created in the store and then symlinked into output folders
-- this creates the folder in the store
createModuleOutputPath :: MimsaConfig -> Backend -> IO FilePath
createModuleOutputPath mimsaConfig be =
  getStoreFolder mimsaConfig (transpiledModuleOutputPath be)

-- all files are created in the store and then symlinked into output folders
-- this creates the folder in the store
createIndexOutputPath :: MimsaConfig -> Backend -> IO FilePath
createIndexOutputPath mimsaConfig be =
  getStoreFolder mimsaConfig (transpiledIndexOutputPath be)

-- all files are created in the store and then symlinked into output folders
-- this creates the folder in the store
createStdlibOutputPath :: MimsaConfig -> Backend -> IO FilePath
createStdlibOutputPath mimsaConfig be =
  getStoreFolder mimsaConfig (transpiledStdlibOutputPath be)

getStdlib :: Backend -> Text
getStdlib CommonJS = coerce commonJSStandardLibrary

-- given output type and list of expressions, copy everything to local
-- folder for output in repl
copyLocalOutput ::
  MimsaConfig ->
  Backend ->
  Set ExprHash ->
  ExprHash ->
  ExceptT StoreError IO Text
copyLocalOutput mimsaConfig be exprHashes rootExprHash = do
  modulePath <- liftIO $ createModuleOutputPath mimsaConfig be
  stdlibPath <- liftIO $ createStdlibOutputPath mimsaConfig be
  indexPath <- liftIO $ createIndexOutputPath mimsaConfig be
  outputPath <- liftIO $ createOutputFolder be rootExprHash
  -- link modules
  traverse_ (copyModule modulePath outputPath be) exprHashes
  -- link stdlib
  _ <- copyStdlib stdlibPath outputPath be
  -- link index
  copyIndex indexPath outputPath be

copyModule :: FilePath -> FilePath -> Backend -> ExprHash -> ExceptT StoreError IO ()
copyModule modulePath outputPath be exprHash = do
  let filename = moduleFilename be exprHash
      fromPath = modulePath <> T.unpack filename
      toPath = outputPath <> T.unpack filename
  tryCopy fromPath toPath

-- the stdlib is already in the store so we copy it to the target folder
copyStdlib :: FilePath -> FilePath -> Backend -> ExceptT StoreError IO Text
copyStdlib stdlibPath outputPath be = do
  let fromPath = T.pack stdlibPath <> stdLibFilename be
  let toPath = T.pack outputPath <> stdLibFilename be
  tryCopy (T.unpack fromPath) (T.unpack toPath)
  pure toPath

-- the index is already in ths store so we copy it to the target folder
copyIndex :: FilePath -> FilePath -> Backend -> ExceptT StoreError IO Text
copyIndex indexPath outputPath be = do
  let filename = T.unpack $ indexFilename be
      fromPath = indexPath <> filename
      toPath = outputPath <> filename
  tryCopy fromPath toPath
  pure (T.pack toPath)
