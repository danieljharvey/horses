{-# LANGUAGE OverloadedStrings #-}

module Test.Transform.Inliner
  ( spec,
  )
where

import Language.Mimsa.Transform.Inliner
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "Inliner" $ do
    describe "shouldInline" $ do
      it "Yes to number literal" $ do
        shouldInline (unsafeParseExpr "1")
          `shouldBe` True
      it "Yes to string literal" $ do
        shouldInline (unsafeParseExpr "\"dog\"")
          `shouldBe` True
      it "Yes to bool literal" $ do
        shouldInline (unsafeParseExpr "True")
          `shouldBe` True
      it "Yes to number array literal" $
        do
          shouldInline (unsafeParseExpr "[1,2,3]")
          `shouldBe` True
      it "Yes to record full of literals" $ do
        shouldInline (unsafeParseExpr "{ a: 1, b: True, c: \"dog\", d: [1,2,3] }")
          `shouldBe` True
      it "No to function" $ do
        shouldInline (unsafeParseExpr "\\a -> True")
          `shouldBe` False
