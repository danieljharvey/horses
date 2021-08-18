{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.RunNode (spec, runScriptInline, runScriptFromFile, lbsToString) where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.Exit
import System.Process.Typed
import Test.Hspec

runScriptInline :: (MonadIO m) => String -> m (ExitCode, String)
runScriptInline script = do
  (ec, success, failure) <- readProcess (proc "node" ["-p", script])
  case ec of
    ExitSuccess -> pure (ec, binLastLine success)
    ExitFailure _ -> pure (ec, binLastLine failure)

runScriptFromFile :: (MonadIO m) => String -> m (ExitCode, String)
runScriptFromFile filename = do
  (ec, success, failure) <- readProcess (proc "node" [filename])
  case ec of
    ExitSuccess -> pure (ec, binNewline success)
    ExitFailure _ -> pure (ec, binNewline failure)

lbsToString :: LBS.ByteString -> String
lbsToString = T.unpack . decodeUtf8 . LBS.toStrict

binNewline :: LBS.ByteString -> String
binNewline = init . lbsToString

binLastLine :: LBS.ByteString -> String
binLastLine = mconcat . init . lines . lbsToString

spec :: Spec
spec = do
  describe "RunNode" $ do
    describe "runScriptInline" $ do
      it "Fails with ExitFailure code" $ do
        (ec, _) <- runScriptInline "sdfsfgjljlksj4r34"
        ec `shouldBe` ExitFailure 1
      it "Succeeds with printed value" $ do
        (ec, bs) <- runScriptInline "console.log('egg')"
        ec `shouldBe` ExitSuccess
        bs `shouldBe` "egg"

    describe "runScriptFromFile" $ do
      it "Succeeds with printed value" $ do
        (ec, bs) <- runScriptFromFile "static/test/test.js"
        ec `shouldBe` ExitSuccess
        bs `shouldBe` "i am a test"
