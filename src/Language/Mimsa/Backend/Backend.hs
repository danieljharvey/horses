{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Backend
  ( outputCommonJS,
    goCompile,
    getStdlib,
    copyLocalOutput,
    Backend (..),
  )
where

import Control.Monad.Except
import Data.Coerce
import Data.Foldable (traverse_)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Server.EnvVars
import Language.Mimsa.Store.ResolvedDeps
import Language.Mimsa.Store.Storage
  ( getStoreExpressionHash,
    getStoreFolder,
    tryCopy,
  )
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

transpileStoreExpression ::
  (Monoid ann) =>
  MimsaConfig ->
  Backend ->
  Store ann ->
  StoreExpression ann ->
  IO FilePath
transpileStoreExpression mimsaConfig be store' se = do
  outputFolderPath <- createModuleOutputPath mimsaConfig be
  let filename = moduleFilename be (getStoreExpressionHash se)
  let path = T.pack outputFolderPath <> filename
  exists <-
    doesFileExist
      (T.unpack path)
  if exists
    then T.putStrLn $ path <> " already exists"
    else case resolveTypeDeps store' (storeTypeBindings se) of
      Left _ -> error "could not resolve types for output"
      Right dataTypes ->
        do
          let jsOutput = outputCommonJS dataTypes se
          T.putStrLn $ "Writing " <> path <> "..."
          T.writeFile (T.unpack path) (coerce jsOutput)
  pure
    (T.unpack path)

-- we write the index to the store and then copy it
createIndexFile :: MimsaConfig -> Backend -> ExprHash -> IO Text
createIndexFile mimsaConfig CommonJS rootExprHash = do
  storePath <- createIndexOutputPath mimsaConfig CommonJS
  outputPath <- createOutputFolder CommonJS rootExprHash
  let outputContent = outputIndexFile CommonJS rootExprHash
      filename = T.unpack $ indexFilename CommonJS
      fromPath = storePath <> filename
      toPath = outputPath <> filename
  T.writeFile fromPath outputContent
  _ <- runExceptT $ tryCopy fromPath toPath
  pure (T.pack toPath)

getStdlib :: Backend -> Text
getStdlib CommonJS = coerce commonJSStandardLibrary

-- we write the stdlib to the store and then copy it
writeStdlib :: MimsaConfig -> Backend -> ExprHash -> IO ()
writeStdlib mimsaConfig CommonJS rootExprHash = do
  storePath <- createStdlibOutputPath mimsaConfig CommonJS
  outputPath <- createOutputFolder CommonJS rootExprHash
  let fromPath = T.pack storePath <> stdLibFilename CommonJS
  T.writeFile (T.unpack fromPath) (getStdlib CommonJS)
  let toPath = T.pack outputPath <> stdLibFilename CommonJS
  _ <- runExceptT $ tryCopy (T.unpack fromPath) (T.unpack toPath)
  pure ()

-- this recreates the folders which i dislike, however, yolo
copyOutput :: MimsaConfig -> Set (StoreExpression ann) -> Backend -> ExprHash -> IO ()
copyOutput mimsaConfig list CommonJS rootExprHash =
  do
    storePath <- createModuleOutputPath mimsaConfig CommonJS
    outputPath <- createOutputFolder CommonJS rootExprHash
    let doLink se = do
          let filename = moduleFilename CommonJS (getStoreExpressionHash se)
              fromPath = storePath <> T.unpack filename
              toPath = outputPath <> T.unpack filename
          runExceptT $ tryCopy fromPath toPath
    traverse_ doLink list

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

goCompile ::
  (Ord ann, Monoid ann) =>
  MimsaConfig ->
  Backend ->
  Store ann ->
  StoreExpression ann ->
  IO Text
goCompile mimsaConfig be store' se = do
  let list = getTranspileList store' se
  let rootExprHash = getStoreExpressionHash se
  -- create output files in store if they don't exist
  traverse_ (transpileStoreExpression mimsaConfig be store') list
  _ <- transpileStoreExpression mimsaConfig be store' se
  -- create index file in output folder
  outputPath <- createIndexFile mimsaConfig be rootExprHash
  -- write stdlib file in output folder
  writeStdlib mimsaConfig be rootExprHash
  -- copy all the files
  copyOutput mimsaConfig (list <> S.singleton se) be rootExprHash
  -- return path of main filename
  pure outputPath
