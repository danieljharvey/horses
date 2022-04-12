{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.ZipFile
  ( createZipFile,
    storeZipFile,
    encodeZipFile,
  )
where

import qualified Codec.Archive.Zip as Zip
import Control.Monad.Except
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Store.RootPath
import System.Directory

------

textToLBS :: Text -> LBS.ByteString
textToLBS = LBS.fromChunks . return . T.encodeUtf8

zipEntry :: String -> Text -> Zip.Entry
zipEntry filename input =
  Zip.toEntry
    ("./" <> filename)
    0
    (textToLBS input)

-- each expression is symlinked from the store to ./output/<exprhash>/<filename.ext>
createZipFolder :: (MonadIO m) => Backend -> ExprHash -> m FilePath
createZipFolder be exprHash = do
  let outputPath = zipFileOutputPath be
  let path = outputPath <> "/" <> show exprHash
  liftIO $ createDirectoryIfMissing True path
  pure (path <> "/")

moduleEntry ::
  (MonadIO m) =>
  FilePath ->
  Backend ->
  ExprHash ->
  m Zip.Entry
moduleEntry modulePath be exprHash = do
  let filename = T.unpack $ moduleFilename be exprHash <> fileExtension be
      fromPath = modulePath <> filename
  input <- liftIO (T.readFile fromPath)
  pure (zipEntry ("./" <> filename) input)

indexEntry ::
  (MonadIO m) =>
  FilePath ->
  Backend ->
  ExprHash ->
  m Zip.Entry
indexEntry indexPath be rootExprHash = do
  let filename = T.unpack $ indexFilename be rootExprHash
      fromPath = indexPath <> filename
      outputFilename = T.unpack (indexOutputFilename be rootExprHash)
  input <- liftIO (T.readFile fromPath)
  pure (zipEntry ("./" <> outputFilename) input)

-- create zip archive that can be saved as a file or returned through server
createZipFile ::
  (MonadIO m) =>
  RootPath ->
  Backend ->
  Set ExprHash ->
  ExprHash ->
  m Zip.Archive
createZipFile rootPath be exprHashes rootExprHash = do
  modulePath <- createModuleOutputPath rootPath be
  indexPath <- createIndexOutputPath rootPath be
  stdlibPath <- createStdlibOutputPath rootPath be
  -- create entries
  modules <-
    traverse
      (moduleEntry modulePath be)
      (S.toList exprHashes)
  index <- indexEntry indexPath be rootExprHash
  stdlib <- stdlibEntry stdlibPath be
  pure (createArchive $ modules <> [index] <> [stdlib])

-- the stdlib is already in the store so we copy it to the target folder
stdlibEntry :: (MonadIO m) => FilePath -> Backend -> m Zip.Entry
stdlibEntry stdlibPath be = do
  let filename = T.unpack $ stdlibFilename be <> fileExtension be
  let fromPath = stdlibPath <> filename
  input <- liftIO (T.readFile fromPath)
  pure (zipEntry ("./" <> filename) input)

-- write zip file to a given file path
storeZipFile :: (MonadIO m) => Backend -> ExprHash -> Zip.Archive -> m FilePath
storeZipFile be rootExprHash archive = do
  zipPath <- createZipFolder be rootExprHash
  let filename = "output.zip"
  let path = zipPath <> filename
  liftIO $ LB.writeFile path (encodeZipFile archive)
  pure path

encodeZipFile :: Zip.Archive -> LB.ByteString
encodeZipFile = encode

createArchive :: [Zip.Entry] -> Zip.Archive
createArchive = foldr Zip.addEntryToArchive Zip.emptyArchive
