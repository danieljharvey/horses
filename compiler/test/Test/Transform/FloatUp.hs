{-# LANGUAGE OverloadedStrings #-}

module Test.Transform.FloatUp
  ( spec,
  )
where

import Language.Mimsa.Transform.FloatUp
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "FloatUp" $ do
    it "Does nothing when no pattern match" $ do
      let expr = unsafeParseExpr "let a = True in 1"
      floatUp expr `shouldBe` expr
    it "Does nothing when let uses lambda variable" $ do
      let expr = unsafeParseExpr "\\b -> let a = b + 1; a + b"
      floatUp expr `shouldBe` expr
    it "Pushes a let above a lambda" $ do
      let expr = unsafeParseExpr "\\b -> let a = 1; a + b"
          expected = unsafeParseExpr "let a = 1; \\b -> a + b"
      floatUp expr `shouldBe` expected
    it "Pushes a let up once but not twice" $ do
      let expr = unsafeParseExpr "\\a -> \\c -> let b = a + 1; a + b + c"
          expected = unsafeParseExpr "\\a -> let b = a + 1; \\c -> a + b + c"
      floatUp expr `shouldBe` expected
    it "Pushes a let up twice" $ do
      let expr = unsafeParseExpr "\\b -> \\c -> let a = 1; a + b + c"
          expected = unsafeParseExpr "let a = 1; \\b -> \\c -> a + b + c"
      floatUp expr `shouldBe` expected
