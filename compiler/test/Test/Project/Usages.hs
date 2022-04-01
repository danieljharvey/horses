{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Project.Usages
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
      findUsages testStdlib (exprHash 6) `shouldBe` Right mempty
    it "Finds direct and transient uses of array" $ do
      let stateHash = getHashOfName testStdlib "array"
      findUsages testStdlib stateHash
        `shouldSatisfy` \case
          (Right items) -> not (null items)
          _ -> error "oh no"
