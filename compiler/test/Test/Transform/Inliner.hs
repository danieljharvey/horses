{-# LANGUAGE OverloadedStrings #-}

module Test.Transform.Inliner
  ( spec,
  )
where

import Data.Maybe
import Debug.Trace
import Language.Mimsa.Transform.Inliner
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  fdescribe "Inliner" $ do
    describe "howTrivial" $ do
      it "Yes to number literal" $ do
        howTrivial (unsafeParseExpr "1")
          `shouldSatisfy` isJust
      it "Yes to string literal" $ do
        howTrivial (unsafeParseExpr "\"dog\"")
          `shouldSatisfy` isJust
      it "Yes to bool literal" $ do
        howTrivial (unsafeParseExpr "True")
          `shouldSatisfy` isJust
      it "Yes to number array literal" $
        do
          howTrivial (unsafeParseExpr "[1,2,3]")
          `shouldSatisfy` isJust
      it "Yes to record full of literals" $ do
        howTrivial (unsafeParseExpr "{ a: 1, b: True, c: \"dog\", d: [1,2,3] }")
          `shouldSatisfy` isJust
      it "Yes to var" $ do
        howTrivial (unsafeParseExpr "b")
          `shouldSatisfy` isJust
      it "No to function" $ do
        howTrivial (unsafeParseExpr "\\a -> True")
          `shouldBe` Nothing
    describe "inlineInternal" $ do
      it "Does nothing when no vars" $ do
        let expr = unsafeParseExpr "True"
        inlineInternal expr
          `shouldBe` expr
      it "Inlines simple literal that is used once" $ do
        let expr = unsafeParseExpr "let a = 1 in a"
            expected = unsafeParseExpr "let a = 1 in 1"
        inlineInternal expr
          `shouldBe` expected
      it "Inline function when it is used once" $ do
        let expr = unsafeParseExpr "let a = \\b -> 1 in a"
            expected = unsafeParseExpr "let a = \\b -> 1 in \\b -> 1"
        inlineInternal expr
          `shouldBe` expected
      it "Does not inlines trivial item into function" $ do
        let expr = unsafeParseExpr "let a = 1 in \\f -> g True a"
        inlineInternal expr
          `shouldBe` expr
      it "Inline recursive function" $ do
        let expr = unsafeParseExpr "let dec a = if a == 0 then 0 else dec (a - 1); dec 9"
            expected = unsafeParseExpr "let dec a = if a == 0 then 0 else dec (a - 1); (\\a -> if a == 0 then 0 else dec (a - 1)) 9"
        inlineInternal expr
          `shouldBe` expected
      it "Function with type annotation" $ do
        let expr = unsafeParseExpr "let identity = \\(abc: a) -> abc; identity True"
            expected = traceShowId $ unsafeParseExpr "let identity = \\(abc: a) -> abc; (\\(abc: a) -> abc) True"
        inlineInternal expr
          `shouldBe` expected
