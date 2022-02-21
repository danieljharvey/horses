{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Transform.FindUses
  ( spec,
  )
where

import qualified Data.Map as M
import Data.Monoid
import Language.Mimsa.Transform.FindUses
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
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
        `shouldBe` Uses (M.singleton "a" (Sum 1))
    it "Does not find uses of a var in it's own recursive def" $ do
      findUses (unsafeParseExpr "let a b = if b == 0 then 0 else a (b - 1) in a")
        `shouldBe` Uses (M.fromList [("a", Sum 1), ("b", Sum 2)])
