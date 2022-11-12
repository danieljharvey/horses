{-# LANGUAGE OverloadedStrings #-}

module Test.Interpreter.ToHOAS
  ( spec,
  )
where

import Language.Mimsa.Interpreter.ToHOAS
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "ToHOAS" $ do
    describe "There and back again" $ do
      it "Infixes, literals" $ do
        let input = unsafeParseExpr "1 + 2 + 3"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
      it "A function appears" $ do
        let input = unsafeParseExpr "\\a -> a"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
      it "A pattern match appears" $ do
        let input = unsafeParseExpr "match (1,2) with (a,b) -> a"
            result = fromHOAS (toHOAS input)
        result `shouldBe` input
