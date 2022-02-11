{-# LANGUAGE OverloadedStrings #-}

module Test.Transform.FlattenLets
  ( spec,
  )
where

import Language.Mimsa.Transform.FlattenLets
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "FlattenLets" $ do
    it "Does nothing when no nested lets" $ do
      let expr = unsafeParseExpr "let a = True in 1"
      flattenLets expr `shouldBe` expr
    it "Flattens a let" $ do
      let expr = unsafeParseExpr "let a = (let b = 1 in True) in a + 1"
          expected = unsafeParseExpr "let b = 1; let a = True; a + 1"
      flattenLets expr `shouldBe` expected
    it "Flattens many lets" $ do
      let expr = unsafeParseExpr "let a = (let b = (let c = 1 in c) in True) in a + 1"
          expected = unsafeParseExpr "let c = 1; let b = c; let a = True; a + 1"
      flattenLets expr `shouldBe` expected
