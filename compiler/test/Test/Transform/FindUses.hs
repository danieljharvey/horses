{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Transform.FindUses
  ( spec,
  )
where

import qualified Data.HashMap.Strict as M
import Data.Monoid
import Language.Mimsa.Core
import Language.Mimsa.Transform.FindUses
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "FindUses" $ do
    it "Nothing in literal" $ do
      findUses @Name @Annotation (bool True)
        `shouldBe` mempty
    it "One in var uses" $ do
      findUses (unsafeParseExpr "let a = 1 in a")
        `shouldBe` Uses (M.singleton (Nothing, "a") (Sum 1))
    it "Does not find uses of a var in it's own recursive def" $ do
      findUses (unsafeParseExpr "let a b = if b == 0 then 0 else a (b - 1) in a")
        `shouldBe` Uses (M.fromList [((Nothing, "a"), Sum 1), ((Nothing, "b"), Sum 2)])
