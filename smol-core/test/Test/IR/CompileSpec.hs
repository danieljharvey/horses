{-# LANGUAGE OverloadedStrings #-}

module Test.IR.CompileSpec (spec) where

import qualified Compile.RunLLVM as Run
import Data.Text (Text)
import LLVM.AST hiding (function)
import Test.Hspec
import Test.IR.RawSamples

-- run the code, get the output, die
run :: Module -> IO Text
run = fmap Run.rrResult . Run.run

spec :: Spec
spec = do
  describe "Compile" $ do
    describe "Examples" $ do
      it "Compiles and runs an example module" $ do
        resp <- run print42
        resp `shouldBe` "42"
      it "Uses id function" $ do
        resp <- run useId42
        resp `shouldBe` "42"
      it "Uses add function" $ do
        resp <- run useAdd42
        resp `shouldBe` "42"
      it "Uses const function (curried)" $ do
        resp <- run useConst42Curried
        resp `shouldBe` "42"
      it "Makes and deconstructs a one tuple" $ do
        resp <- run oneTuple42
        resp `shouldBe` "42"
      it "Makes and deconstructs a two tuple" $ do
        resp <- run twoTuple42
        resp `shouldBe` "42"
      it "Makes and deconstructs a nested two tuple" $ do
        resp <- run nestedTuple42
        resp `shouldBe` "42"
      it "Basic if statement" $ do
        resp <- run useBasicIf
        resp `shouldBe` "1"
      it "Makes and deconstructs a sum type" $ do
        resp <- run either42
        resp `shouldBe` "42"
