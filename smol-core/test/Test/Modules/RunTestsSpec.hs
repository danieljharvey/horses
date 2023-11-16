{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.RunTestsSpec (spec) where

import qualified Data.Text as T
import Smol.Core
import Smol.Core.Modules.Check
import Smol.Core.Modules.RunTests
import Smol.Core.Modules.Types
import Smol.Core.Modules.Types.ModuleError
import Test.Helpers
import Test.Hspec

-- this should probably become shared code
testTypecheck ::
  T.Text ->
  Either
    (ModuleError Annotation)
    (Module ResolvedDep (Type ResolvedDep Annotation))
testTypecheck input =
  case parseModuleAndFormatError input of
    Right moduleParts -> checkModule input moduleParts
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
        let typedMod =
              testTypecheck
                ( joinText
                    [ "def yes = True",
                      "def no = False",
                      "test \"pass\" { yes }",
                      "test \"fail\" { no }"
                    ]
                )
        runTests <$> typedMod `shouldBe` Right [("fail", False), ("pass", True)]

      it "Two tests, one pass, one fail, with deps" $ do
        let typedMod =
              testTypecheck
                ( joinText
                    [ "def id a = a",
                      "def yes = id True",
                      "def no = id False",
                      "test \"pass\" { yes }",
                      "test \"fail\" { no }"
                    ]
                )
        runTests <$> typedMod `shouldBe` Right [("fail", False), ("pass", True)]
