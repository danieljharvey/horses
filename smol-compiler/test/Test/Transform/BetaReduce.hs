{-# LANGUAGE OverloadedStrings #-}

module Test.Transform.BetaReduce
  ( spec,
  )
where

import Language.Mimsa.Transform.BetaReduce
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "BetaReduce" $ do
    it "Does nothing when no pattern match" $ do
      let expr = unsafeParseExpr "let a = True in 1"
      betaReduce expr `shouldBe` expr
    it "Turns app lambda val in let val" $ do
      let expr = unsafeParseExpr "(\\x -> x + 2) 3"
          expected = unsafeParseExpr "let x = 3 in x + 2"
      betaReduce expr `shouldBe` expected
    it "Turns app lambda val in let val without type signature" $ do
      let expr = unsafeParseExpr "(\\x -> x : a -> a) 3"
          expected = unsafeParseExpr "let x = 3 in x"
      betaReduce expr `shouldBe` expected
    it "Removes redundant if statement" $ do
      let expr = unsafeParseExpr "if True then 1 else 2"
          expected = unsafeParseExpr "1"
      betaReduce expr `shouldBe` expected
    it "Removes redundant if statement (2)" $ do
      let expr = unsafeParseExpr "if False then 1 else 2"
          expected = unsafeParseExpr "2"
      betaReduce expr `shouldBe` expected
    it "Removes record-and-record-get" $ do
      let expr = unsafeParseExpr "{ dog: True }.dog"
          expected = unsafeParseExpr "True"
      betaReduce expr `shouldBe` expected
