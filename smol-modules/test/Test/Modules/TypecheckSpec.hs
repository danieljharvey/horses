{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Modules.TypecheckSpec (spec) where

import Data.Bifunctor (second)
import Data.Either (isRight)
import Data.FileEmbed
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Smol.Core
import Smol.Modules.Check
import Smol.Modules.Parser
import Smol.Modules.Types hiding (Entity (..))
import Smol.Modules.Types.ModuleError
import Smol.Typecheck.Types
import Test.Helpers
import Test.Hspec

-- these are saved in a file that is included in compilation
testInputs :: [(FilePath, Text)]
testInputs =
  fmap (second T.decodeUtf8) $(makeRelativeToProject "test/static/" >>= embedDir)

testTypecheck ::
  Text ->
  Either
    (ModuleError Annotation)
    (Module ResolvedDep (Type ResolvedDep Annotation))
testTypecheck input =
  case parseModuleAndFormatError input of
    Right moduleParts -> checkModule input moduleParts
    Left e -> error (show e)

spec :: Spec
spec = do
  describe "Modules" $ do
    describe "Tests" $ do
      it "Accepts a unit test with type `Boolean`" $ do
        testTypecheck
          ( joinText
              [ "test \"it's fine\" { yes }",
                "def yes: Bool { True }"
              ]
          )
          `shouldSatisfy` isRight

      it "Does not accept a unit test with another type" $ do
        let input =
              joinText
                [ "test \"it's fine\" { yes }",
                  "def yes : Int { 100 }"
                ]
        testTypecheck input
          `shouldSatisfy` \case
            Left
              ( ErrorInTest
                  "it's fine"
                  ( TestDoesNotTypecheck
                      _
                      (TCTypeMismatch _ _)
                    )
                ) -> True
            _ -> False

    describe "Typecheck" $ do
      let typecheckAModule (filepath, input) =
            it ("Typechecks " <> filepath <> " successfully") $
              testTypecheck input `shouldSatisfy` isRight
      traverse_ typecheckAModule testInputs
