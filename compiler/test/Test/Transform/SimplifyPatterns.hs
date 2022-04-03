{-# LANGUAGE OverloadedStrings #-}

module Test.Transform.SimplifyPatterns
  ( spec,
  )
where

import Language.Mimsa.Transform.SimplifyPatterns
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "SimplifyPatterns" $ do
    describe "Pattern match" $ do
      it "Does nothing when no constructor in expr" $ do
        let expr = unsafeParseExpr "match a with (Just b) -> True | Nothing -> False"
        simplifyPatterns expr `shouldBe` expr
      it "Removes constructors and useless patterns" $ do
        let expr = unsafeParseExpr "match Just a with (Just b) -> True | Nothing -> False"
            expected = unsafeParseExpr "match a with b -> True"
        simplifyPatterns expr `shouldBe` expected
      -- if we had variable length tuples this could be OK though
      it "Does not work with constructors with more than two args" $ do
        let expr = unsafeParseExpr "match Triple 1 2 3 with (Triple a b c) -> True | _ -> False"
        simplifyPatterns expr `shouldBe` expr
      it "Converts two arg constructor to use tuple" $ do
        let expr = unsafeParseExpr "match These 1 2 with (These a b) -> True | (This a) -> False | (That b) -> False"
            expected = unsafeParseExpr "match (1, 2) with (a, b) -> True"
        simplifyPatterns expr `shouldBe` expected
