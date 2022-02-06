{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.TailCall
  ( spec,
  )
where

import Language.Mimsa.Backend.Core.TailCall
import Language.Mimsa.Types.Identifiers ()
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "TailCall" $ do
    it "Detects a let with no recursion" $ do
      let expr = unsafeParseExpr "let fn = 1 in True"
          result = isTailRecursive expr
      result `shouldBe` NoRecursion
    it "Detects a let with tail recursion" $ do
      let expr = unsafeParseExpr "let fn a = \\value -> \\count -> if count == 0 then value else fn (value + 1) (count - 1)"
          result = isTailRecursive expr
      result `shouldBe` TailRecursion
