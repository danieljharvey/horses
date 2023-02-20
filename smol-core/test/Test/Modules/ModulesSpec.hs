{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Modules.ModulesSpec (spec) where

import Data.Either (isRight)
import Data.List (isInfixOf)
import Data.Bifunctor (second)
import Data.FileEmbed
import Data.Foldable (find)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Test.Hspec
import Smol.Core
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.Typecheck

-- these are saved in a file that is included in compilation
testInputs :: [(FilePath, Text)]
testInputs =
  fmap (second T.decodeUtf8) $(makeRelativeToProject "test/static/" >>= embedDir)

preludeInput :: Text
preludeInput = case find (\(filename,_) -> "Prelude" `isInfixOf` filename) testInputs of
                 Just (_,code) -> code
                 Nothing -> error "could not find prelude code"

spec :: Spec
spec = do
  fdescribe "Modules" $ do
    describe "Typecheck" $ do
      it "Typechecks Prelude successfully" $ do
        case parseModuleAndFormatError preludeInput of
          Right moduleParts -> do
              case moduleFromModuleParts mempty moduleParts of
                Left e -> error (show e)
                Right myModule -> do
                  let modules = M.singleton 123 myModule
                  typecheckAllModules modules `shouldSatisfy` isRight
          Left e -> error (show e)
