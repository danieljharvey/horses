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
        shouldInline 0 (unsafeParseExpr "1")
          `shouldBe` True
      it "Yes to string literal" $ do
        shouldInline 0 (unsafeParseExpr "\"dog\"")
          `shouldBe` True
      it "Yes to bool literal" $ do
        shouldInline 0 (unsafeParseExpr "True")
          `shouldBe` True
      it "Yes to number array literal" $
        do
          shouldInline 0 (unsafeParseExpr "[1,2,3]")
          `shouldBe` True
      it "Yes to record full of literals" $ do
        shouldInline 0 (unsafeParseExpr "{ a: 1, b: True, c: \"dog\", d: [1,2,3] }")
          `shouldBe` True
      it "No to function" $ do
        shouldInline 0 (unsafeParseExpr "\\a -> True")
          `shouldBe` False
    describe "inline" $ do
      it "Does nothing when no vars" $ do
        let expr = unsafeParseExpr "True"
        inline expr
          `shouldBe` expr
      it "Inlines simple literal that is used once" $ do
        let expr = unsafeParseExpr "let a = 1 in a"
            expected = unsafeParseExpr "let a = 1 in 1"
        inline expr
          `shouldBe` expected
      it "Don't inline literal used 5 times" $ do
        let expr = unsafeParseExpr "let a = 1 in [a,a,a,a,a]"
        inline expr `shouldBe` expr
      it "Don't inline function" $ do
        let expr = unsafeParseExpr "let a = \\b -> 1 in a"
        inline expr
          `shouldBe` expr
