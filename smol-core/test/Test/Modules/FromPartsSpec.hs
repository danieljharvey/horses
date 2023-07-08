{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.FromPartsSpec (spec) where

import Smol.Core.Modules.FromParts
import Test.Helpers
import Test.Hspec
import qualified Data.Text as T
import Smol.Core.Modules.ModuleError

joinText :: [T.Text] -> T.Text
joinText = T.intercalate "\n"

-- | tests for checking modules make sense

spec :: Spec
spec = do
  describe "Modules" $ do
    describe "FromParts" $ do
      it "Test that refers to non-existent identifier" $ do
        let modParts = unsafeParseModuleItems (joinText ["test \"horses\" using missing"])
            expected = VarNotFound "missing" 

        moduleFromModuleParts modParts `shouldBe` Left expected

      it "Can't have an empty test name" $ do
        let modParts = unsafeParseModuleItems (joinText ["def yes = True", "test \"\" using yes"])
            expected = EmptyTestName "yes" 

        moduleFromModuleParts modParts `shouldBe` Left expected


