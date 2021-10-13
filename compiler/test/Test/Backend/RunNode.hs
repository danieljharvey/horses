{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.RunNode
  ( spec,
    runScriptFromFile,
    runTypescriptFromFile,
    lbsToString,
    withCache,
  )
where

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.Exit
import System.Process.Typed
import Test.Hspec

-- | Pass a filepath to a JS file for Node to execute.
-- Required as ES modules don't work with the `-p` flag
runScriptFromFile :: (MonadIO m) => String -> m (Bool, String)
runScriptFromFile filename = do
  (ec, success, failure) <- readProcess (proc "node" [filename])
  case ec of
    ExitSuccess -> pure (exitCodeToBool ec, binNewline success)
    ExitFailure _ -> pure (exitCodeToBool ec, binNewline failure)

runTypescriptFromFile :: (MonadIO m) => String -> m (Bool, String)
runTypescriptFromFile filename = do
  (ec, success, failure) <- readProcess (proc "ts-node" [filename])
  case ec of
    ExitSuccess -> pure (exitCodeToBool ec, binNewline success)
    ExitFailure _ -> pure (exitCodeToBool ec, binNewline failure)

exitCodeToBool :: ExitCode -> Bool
exitCodeToBool ExitSuccess = True
exitCodeToBool _ = False

cacheResult :: (MonadIO m) => String -> (Bool, String) -> m ()
cacheResult filename result = do
  let json = JSON.encode result
  liftIO $ LBS.writeFile filename json

-- load previously
loadCacheResult :: (MonadIO m) => String -> m (Maybe (Bool, String))
loadCacheResult filename = do
  res <- liftIO $ try $ LBS.readFile filename
  case (res :: Either IOError LBS.ByteString) of
    Right json -> do
      pure $ JSON.decode json
    Left _ -> pure Nothing

-- | Wrap a test in caching
withCache :: (MonadIO m) => String -> m (Bool, String) -> m (Bool, String)
withCache cachePath action = do
  cached <- loadCacheResult cachePath
  case cached of
    Just res -> pure res
    Nothing -> do
      res <- action
      cacheResult cachePath res
      pure res

lbsToString :: LBS.ByteString -> String
lbsToString = T.unpack . decodeUtf8 . LBS.toStrict

binNewline :: LBS.ByteString -> String
binNewline = init . lbsToString

spec :: Spec
spec = do
  describe "RunNode" $ do
    describe "runScriptFromFile" $ do
      it "Succeeds with printed value" $ do
        (ec, bs) <- runScriptFromFile "static/test/test.js"
        ec `shouldBe` True
        bs `shouldBe` "i am a test"

    describe "runTypescriptFromFile" $ do
      it "Succeeds with printed value" $ do
        (ec, bs) <- runTypescriptFromFile "static/test/test.ts"
        ec `shouldBe` True
        bs `shouldBe` "i am a test"

      it "Fails with badly-typed file" $ do
        (ec, _) <- runTypescriptFromFile "static/test/failing-test.ts"
        ec `shouldBe` False
