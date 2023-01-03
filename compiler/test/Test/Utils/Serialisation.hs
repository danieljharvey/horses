{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Utils.Serialisation
  ( getAllFilesInDir,
    loadJSON,
    saveJSON,
    savePretty,
    loadRegression,
    saveRegression,
    createOutputFolder,
    deleteOutputFolder,
    loadStoreExpression,
    saveStoreExpression,
  )
where

import Control.Exception (try)
import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Functor
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding
import Language.Mimsa.Core
import Language.Mimsa.Store
import Language.Mimsa.Types.Store
import System.Directory

saveRootPath :: String
saveRootPath = "./compiler/test/golden"

createOutputFolder :: FilePath -> IO FilePath
createOutputFolder folder = do
  let fullPath = saveRootPath <> "/" <> folder
  createDirectoryIfMissing True fullPath
  pure (fullPath <> "/")

deleteOutputFolder :: FilePath -> IO ()
deleteOutputFolder folder = do
  let fullPath = saveRootPath <> "/" <> folder
  removePathForcibly fullPath

getAllFilesInDir :: FilePath -> String -> IO [String]
getAllFilesInDir folder ext = do
  path <- createOutputFolder folder
  files <- listDirectory path
  pure $
    filter
      (isInfixOf ext)
      ((path <>) <$> files)

-- if file does not already exist, convert it to ByteString and save it
saveRegression ::
  String ->
  (a -> ByteString) ->
  a ->
  IO ()
saveRegression savePath convert a = do
  exists <- doesFileExist savePath
  if exists
    then pure ()
    else do
      putStrLn $ "Writing " <> savePath <> "..."
      BS.writeFile savePath (convert a)

-- attempt to load and decode from file
loadRegression ::
  String ->
  (ByteString -> Either Text a) ->
  IO (Either Text a)
loadRegression loadPath decode = do
  file <- try $ BS.readFile loadPath
  case file of
    Right a -> case decode a of
      Right ok -> pure (Right ok)
      Left e -> do
        putStrLn $ loadPath <> ": "
        T.putStrLn e
        pure (Left e)
    Left (_ :: IOError) ->
      pure (Left $ "Error loading file: " <> T.pack loadPath)

saveJSON ::
  (JSON.ToJSON a) =>
  String ->
  a ->
  IO ()
saveJSON filename = saveRegression filename JSON.encode

loadJSON ::
  (JSON.FromJSON a) =>
  String ->
  IO (Either Text a)
loadJSON filename =
  loadRegression
    filename
    ( \a -> case JSON.decode a of
        Just a' -> Right a'
        Nothing -> Left "JSON decode failed"
    )

saveStoreExpression :: String -> StoreExpression ann -> IO ()
saveStoreExpression filename =
  saveRegression filename (fst . serialiseStoreExpression)

loadStoreExpression :: String -> IO (Either Text (StoreExpression ()))
loadStoreExpression filename =
  loadRegression
    filename
    ( \a -> case deserialiseStoreExpression a of
        Just a' -> pure (a' $> ())
        Nothing -> Left "Decoding Store Expression failed"
    )

savePretty ::
  (Printer a) =>
  String ->
  a ->
  IO ()
savePretty filename =
  saveRegression
    filename
    (encodeUtf8 . fromStrict . prettyPrint)
