module Router.Unzip (unzipFiles) where

import Codec.Archive.Zip
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import Router.Config

unzipFiles :: (MonadIO m) => Config -> LBS.ByteString -> m [String]
unzipFiles cfg bs = do
  case toArchiveOrFail bs of
    Left e -> do
      liftIO $ print e
      pure
        mempty
    Right archive -> do
      let options = pure $ OptDestination (cfgVolumePath cfg)
      liftIO $ extractFilesFromArchive options archive
      pure (filesInArchive archive)
