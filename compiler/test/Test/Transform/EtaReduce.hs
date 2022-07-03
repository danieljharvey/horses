{-# LANGUAGE OverloadedStrings #-}

module Test.Transform.EtaReduce
  ( spec,
  )
where

import Language.Mimsa.Transform.EtaReduce
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "EtaReduce" $ do
    it "Does nothing when no function" $ do
      let expr = unsafeParseExpr "let a = True in 1"
      etaReduce expr `shouldBe` expr

    it "Removes unnecessary lambda on single arity function" $ do
      let expr = unsafeParseExpr "\\a -> id a"
          expected = unsafeParseExpr "id"
      etaReduce expr `shouldBe` expected

    it "Removes unnecessary lambda on two arity function" $ do
      let expr = unsafeParseExpr "\\a -> const 1 a"
          expected = unsafeParseExpr "const 1"
      etaReduce expr `shouldBe` expected

    it "Removes two unnecessary lambdas on two arity function" $ do
      let expr = unsafeParseExpr "\\a -> \\b -> const a b"
          expected = unsafeParseExpr "const"
      etaReduce expr `shouldBe` expected
