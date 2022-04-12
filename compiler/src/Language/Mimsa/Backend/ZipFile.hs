{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.ZipFile
  ( zipFromSavedFiles,
    encodeZipFile,
  )
where

import qualified Codec.Archive.Zip as Zip
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Language.Mimsa.Actions.Monad as Actions

------

textToLBS :: Text -> LBS.ByteString
textToLBS = LBS.fromChunks . return . T.encodeUtf8

zipEntry :: String -> Text -> Zip.Entry
zipEntry filename input =
  Zip.toEntry
    ("./" <> filename)
    0
    (textToLBS input)

encodeZipFile :: Zip.Archive -> LB.ByteString
encodeZipFile = encode

createArchive :: [Zip.Entry] -> Zip.Archive
createArchive = foldr Zip.addEntryToArchive Zip.emptyArchive

-- | Given file data from compilation, create a Zip file
zipFromSavedFiles ::
  [(Actions.SavePath, Actions.SaveFilename, Actions.SaveContents)] ->
  Zip.Archive
zipFromSavedFiles entries =
  let zipEntries =
        ( \(_, Actions.SaveFilename filename, Actions.SaveContents contents) ->
            zipEntry (T.unpack filename) contents
        )
          <$> entries
   in createArchive zipEntries
