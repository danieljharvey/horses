{-# LANGUAGE OverloadedStrings #-}

module Test.Transform.FloatDown
  ( spec,
  )
where

import Language.Mimsa.Transform.FloatDown
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "FloatDown" $ do
    it "Does nothing when no pattern match" $ do
      let expr = unsafeParseExpr "let a = True in 1"
      floatDown expr `shouldBe` expr
    it "Pushes a let into each pattern branch" $ do
      let expr = unsafeParseExpr "let a = 1 in match True with True -> 1 | False -> 2"
          expected = unsafeParseExpr "match True with True -> let a = 1 in 1 | False -> let a = 1 in 2"
      floatDown expr `shouldBe` expected
    it "Does not pushes a let when it matches on the let value" $ do
      let expr = unsafeParseExpr "let a = 1 in match a with True -> 1 | False -> 2"
      floatDown expr `shouldBe` expr
    it "Pushes multiple lets into each pattern branch" $ do
      let expr = unsafeParseExpr "let a = 1; let b = a + 1 in match True with True -> 1 | False -> 2"
          expected = unsafeParseExpr "match True with True -> let a = 1; let b = a + 1 in 1 | False -> let a = 1; let b = a + 1 in 2"
      floatDown expr `shouldBe` expected
