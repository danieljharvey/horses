{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | test evaluating and running tests for a module
module Test.Modules.CheckSpec (spec) where
import Smol.Core.Modules.Parser

import Data.Bifunctor (second)
import Data.FileEmbed
import Data.Foldable (traverse_)
import Data.Functor
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Smol.Core
import Smol.Core.Modules.Check
import Smol.Core.Modules.RunTests
import Smol.Core.Modules.Types.ModuleError
import Test.Hspec

-- these are saved in a file that is included in compilation
testInputs :: [(FilePath, Text)]
testInputs =
  fmap (second T.decodeUtf8) $(makeRelativeToProject "test/static/" >>= embedDir)

testsAllPass :: [(a, Bool)] -> Bool
testsAllPass = getAll . foldMap (All . snd)

-- read a file, check if it is OK etc
testCheckModule :: Text -> Either (ModuleError Annotation) ()
testCheckModule input =
  case parseModuleAndFormatError input of
    Left e -> error (show e)
    Right moduleParts ->
      checkModule input moduleParts <&> \tcModule ->
        let testResults = runTests tcModule
         in if testsAllPass testResults
              then ()
              else error (show testResults)

spec :: Spec
spec = do
  describe "Modules" $ do
    describe "Check" $ do
      let testModule (filepath, input) =
            it ("Checks " <> filepath <> " successfully") $
              testCheckModule input `shouldBe` Right ()
      traverse_ testModule testInputs
