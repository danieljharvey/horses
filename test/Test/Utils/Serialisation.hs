{-# LANGUAGE ScopedTypeVariables #-}

module Test.Utils.Serialisation
  ( getAllFilesInDir,
    loadJSON,
    saveJSON,
    savePretty,
    loadRegression,
    saveRegression,
    createOutputFolder,
  )
where

import Control.Exception (try)
import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding
import Language.Mimsa.Printer
import System.Directory

saveRootPath :: String
saveRootPath = "./test/golden"

createOutputFolder :: FilePath -> IO FilePath
createOutputFolder folder = do
  let fullPath = saveRootPath <> "/" <> folder
  createDirectoryIfMissing True fullPath
  pure (fullPath <> "/")

getAllFilesInDir :: FilePath -> IO [String]
getAllFilesInDir folder = do
  path <- createOutputFolder folder
  listDirectory path

-- if file does not already exist, convert it to ByteString and save it
saveRegression :: String -> (a -> ByteString) -> a -> IO ()
saveRegression savePath convert a = do
  exists <- doesFileExist savePath
  if exists
    then pure ()
    else do
      putStrLn $ "Writing " <> savePath <> "..."
      BS.writeFile savePath (convert a)

-- attempt to load and decode from file
loadRegression :: String -> (ByteString -> Maybe a) -> IO (Maybe a)
loadRegression loadPath decode = do
  file <- try $ BS.readFile loadPath
  case file of
    Right a -> case decode a of
      Just ok -> pure (Just ok)
      _ -> pure Nothing
    Left (_ :: IOError) -> pure Nothing

saveJSON :: (JSON.ToJSON a) => String -> a -> IO ()
saveJSON filename = saveRegression filename JSON.encode

loadJSON :: (JSON.FromJSON a) => String -> IO (Maybe a)
loadJSON filename = loadRegression filename JSON.decode

savePretty :: (Printer a) => String -> a -> IO ()
savePretty filename = saveRegression filename (encodeUtf8 . fromStrict . prettyPrint)
