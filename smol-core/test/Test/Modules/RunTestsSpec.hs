{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.RunTestsSpec (spec) where

import qualified Data.Text as T
import Smol.Core
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.Types.ModuleError
import Test.Helpers
import Test.Hspec
import Smol.Core.Modules.Types
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Modules.RunTests

-- this should probably become shared code
testTypecheck ::
  T.Text ->
  Either
    (ModuleError Annotation)
    (Module ResolvedDep (Type ResolvedDep Annotation))
testTypecheck input =
  case parseModuleAndFormatError input of
    Right moduleParts -> do
      case moduleFromModuleParts moduleParts >>= resolveModuleDeps of
        Left e -> error (show e)
        Right (myModule, deps) -> do
          typecheckModule input myModule deps
    Left e -> error (show e)

-- | tests for checking modules make sense
spec :: Spec
spec = do
  describe "Modules" $ do
    describe "Run tests" $ do
      it "No tests, no results" $ do
        let typedMod = testTypecheck (joinText ["def id a = a"])
        runTests <$> typedMod `shouldBe` Right mempty

      it "Two tests, one pass, one fail, no deps" $ do
        let typedMod = testTypecheck (joinText ["def yes = True", "def no = False", "test \"pass\" using yes", "test \"fail\" using no"])
        runTests <$> typedMod `shouldBe` Right [("pass", True), ("fail", False)]

