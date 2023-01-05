{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Serialisation
  ( spec,
  )
where

import Data.Either (partitionEithers)
import Language.Mimsa.Types.Project
import Test.Hspec
import Test.Utils.Serialisation

catEithers :: [Either e a] -> [a]
catEithers as = snd $ partitionEithers as

spec :: Spec
spec =
  describe "Serialisation" $ do
    it "StoreExpression JSON" $ do
      files <- getAllFilesInDir "StoreExpr" "json"
      loaded <- traverse loadStoreExpression files
      length (catEithers loaded) `shouldBe` length loaded

    it "Project JSON" $ do
      files <- getAllFilesInDir "SaveProject" "json"
      loaded <- traverse (loadJSON @SaveProject) files
      length (catEithers loaded) `shouldBe` length loaded
