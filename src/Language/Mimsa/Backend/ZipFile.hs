{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.ZipFile
  ( createZipFile,
    storeZipFile,
  )
where

import qualified Codec.Archive.Zip as Zip
import Control.Monad.Except
import Data.Binary (encode)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Monad
import Language.Mimsa.Store.Storage (getStoreFolder)
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Store
import System.Directory

------

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

-- each expression is symlinked from the store to ./output/<exprhash>/<filename.ext>
createZipFolder :: Backend -> ExprHash -> MimsaM e FilePath
createZipFolder CommonJS exprHash = do
  let outputPath = zipFileOutputPath CommonJS
  let path = outputPath <> "/" <> show exprHash
  liftIO $ createDirectoryIfMissing True path
  pure (path <> "/")

moduleEntry ::
  FilePath ->
  Backend ->
  ExprHash ->
  MimsaM StoreError Zip.Entry
moduleEntry modulePath be exprHash = do
  let filename = LB.unpack (moduleFilename be exprHash)
      fromPath = modulePath <> filename
  input <- liftIO (LB.readFile fromPath)
  pure (Zip.toEntry ("./" <> filename) 0 input)

indexEntry ::
  FilePath ->
  Backend ->
  ExprHash ->
  MimsaM StoreError Zip.Entry
indexEntry indexPath be rootExprHash = do
  let filename = LB.unpack $ indexFilename be rootExprHash
      fromPath = indexPath <> filename
      outputFilename = LB.unpack (indexOutputFilename be)
  input <- liftIO (LB.readFile fromPath)
  pure (Zip.toEntry ("./" <> outputFilename) 0 input)

-- the stdlib is already in the store so we copy it to the target folder
stdlibEntry :: FilePath -> Backend -> MimsaM StoreError Zip.Entry
stdlibEntry stdlibPath be = do
  let filename = LB.unpack (stdLibFilename be)
  let fromPath = stdlibPath <> filename
  input <- liftIO (LB.readFile fromPath)
  pure (Zip.toEntry ("./" <> filename) 0 input)

createZipFile ::
  Backend ->
  Set ExprHash ->
  ExprHash ->
  MimsaM StoreError Zip.Archive
createZipFile be exprHashes rootExprHash = do
  modulePath <- createModuleOutputPath be
  stdlibPath <- createStdlibOutputPath be
  indexPath <- createIndexOutputPath be
  -- create entries
  modules <- traverse (moduleEntry modulePath be) (S.toList exprHashes)
  index <- indexEntry indexPath be rootExprHash
  stdlib <- stdlibEntry stdlibPath be
  pure (createArchive $ modules <> [index] <> [stdlib])

storeZipFile :: Backend -> ExprHash -> Zip.Archive -> MimsaM StoreError FilePath
storeZipFile be rootExprHash archive = do
  zipPath <- createZipFolder be rootExprHash
  let filename = "output.zip"
  let path = zipPath <> filename
  let bs = encode archive
  liftIO $ LB.writeFile path bs
  pure path

createArchive :: [Zip.Entry] -> Zip.Archive
createArchive = foldr Zip.addEntryToArchive Zip.emptyArchive
