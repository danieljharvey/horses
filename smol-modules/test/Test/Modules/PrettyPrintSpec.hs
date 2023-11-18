{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | test evaluating and running tests for a module
module Test.Modules.PrettyPrintSpec (spec) where

import Data.Bifunctor (second)
import Data.FileEmbed
import Data.Foldable (traverse_)
import Data.Functor
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Smol.Modules.Parser as Parse
import Smol.Modules.PrettyPrint (printModuleParts)
import Smol.Modules.Types.ModuleItem
import Smol.Core.Printer
import Test.Hspec

-- these are saved in a file that is included in compilation
testInputs :: [(FilePath, Text)]
testInputs =
  fmap (second T.decodeUtf8) $(makeRelativeToProject "test/static/" >>= embedDir)

-- read a module, pretty print it, is it the same?
parseModule :: Text -> [ModuleItem ()]
parseModule input =
  case Parse.parseModuleAndFormatError input of
    Left e -> error (show e)
    Right parts -> fmap ($> ()) parts

spec :: Spec
spec = do
  describe "Modules" $ do
    describe "PrettyPrint" $ do
      let printModule (filepath, input) =
            it ("Pretty pretting " <> filepath <> " round trips successfully") $ do
              let parts = parseModule input
                  printed = renderWithWidth 80 $ printModuleParts parts
              let parts2 = parseModule printed
              parts `shouldBe` parts2
      traverse_ printModule testInputs

    describe "PrettyPrint is saved" $ do
      let printModule (filepath, input) =
            it ("Pretty pretting " <> filepath <> " is the same") $ do
              let parts = parseModule input
                  printed = renderWithWidth 80 $ printModuleParts parts
              input `shouldBe` printed
      traverse_ printModule testInputs
