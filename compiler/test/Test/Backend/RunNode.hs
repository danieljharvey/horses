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

runProcessFromFile :: (MonadIO m) => String -> String -> m (Bool, String)
runProcessFromFile binaryName filename = do
  result <- liftIO $ try $ readProcess (proc binaryName [filename])
  case result of
    Right (ExitSuccess, success, _) ->
      pure (exitCodeToBool ExitSuccess, binNewline success)
    Right (ExitFailure exitCode, _, failure) ->
      pure (exitCodeToBool (ExitFailure exitCode), binNewline failure)
    Left e -> pure (False, show (e :: IOException))

-- | Pass a filepath to a JS file for Node to execute.
-- Required as ES modules don't work with the `-p` flag
runScriptFromFile :: (MonadIO m) => String -> m (Bool, String)
runScriptFromFile = runProcessFromFile "node"

-- | Pass a filepath to a TS file for `ts-node` to execute.
runTypescriptFromFile :: (MonadIO m) => String -> m (Bool, String)
runTypescriptFromFile = runProcessFromFile "ts-node"

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
