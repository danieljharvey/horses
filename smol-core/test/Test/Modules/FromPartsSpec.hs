{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.FromPartsSpec (spec) where

import Smol.Core.Modules.FromParts
import Smol.Core.Modules.Types.ModuleError
import Test.Helpers
import Test.Hspec

-- | tests for checking modules make sense
spec :: Spec
spec = do
  describe "Modules" $ do
    describe "FromParts" $ do
      it "Test that refers to non-existent identifier" $ do
        let modParts = unsafeParseModuleItems (joinText ["test \"horses\" using missing"])
            expected = ErrorInResolveDeps $ VarNotFound "missing"

        moduleFromModuleParts modParts `shouldBe` Left expected

      it "Can't have an empty test name" $ do
        let modParts = unsafeParseModuleItems (joinText ["def yes = True", "test \"\" using yes"])
            expected = EmptyTestName "yes"

        moduleFromModuleParts modParts `shouldBe` Left expected

      it "Can't have duplicate typeclasses" $ do
        let modParts =
              unsafeParseModuleItems $
                joinText
                  [ "class Eq { equals : a -> a -> Bool }",
                    "class Eq { eq: a -> Bool }"
                  ]
            expected = DuplicateTypeclass "Eq"

        moduleFromModuleParts modParts `shouldBe` Left expected

      it "Missing typeclass for instance" $ do
        let modParts =
              unsafeParseModuleItems $
                joinText
                  [ "instance Eq Int = \\a -> \\b -> a == b"
                  ]
            expected = MissingTypeclass "Eq"

        moduleFromModuleParts modParts `shouldBe` Left expected
