{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Usages
  ( spec,
  )
where

import Language.Mimsa.Project.Usages
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec

spec :: Spec
spec =
  describe "Usages" $ do
    it "Returns empty when passed nothing" $
      findUsages mempty (ExprHash 6) `shouldBe` Right mempty
    it "Finds all uses of Compose in Stdlib" $
      findUsages stdLib (ExprHash 6) `shouldBe` Right mempty
