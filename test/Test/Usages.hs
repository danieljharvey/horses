{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Usages
  ( spec,
  )
where

import Language.Mimsa.Project.Usages
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec =
  describe "Usages" $ do
    it "Returns empty when passed nothing" $
      findUsages mempty (exprHash 6) `shouldBe` Right mempty
    it "Finds all uses of Compose in Stdlib" $
      findUsages stdLib (exprHash 6) `shouldBe` Right mempty
