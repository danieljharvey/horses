{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Modules.ModulesSpec (spec) where

import Data.Bifunctor (second)
import Data.Either (isRight)
import Data.FileEmbed
import Data.Foldable (find)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Smol.Core
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Types.Module.ModuleHash
import Test.Helpers
import Test.Hspec

-- these are saved in a file that is included in compilation
testInputs :: [(FilePath, Text)]
testInputs =
  fmap (second T.decodeUtf8) $(makeRelativeToProject "test/static/" >>= embedDir)

preludeInput :: Text
preludeInput = case find (\(filename, _) -> "Prelude" `isInfixOf` filename) testInputs of
  Just (_, code) -> code
  Nothing -> error "could not find prelude code"

spec :: Spec
spec = do
  fdescribe "Modules" $ do
    describe "ResolvedDeps" $ do
      it "No deps, marks var as unique" $ do
        let expr = unsafeParseExpr "let a = 123 in a"
            expected =
              ELet
                ()
                (UniqueDefinition "a" 1)
                (nat 123)
                (EVar () (UniqueDefinition "a" 1))
        resolveExprDeps expr `shouldBe` expected
      it "No deps, marks two different `a` values correctly" $ do
        let expr = unsafeParseExpr "let a = 123 in let a = 456 in a"
            expected =
              ELet
                ()
                (UniqueDefinition "a" 1)
                (nat 123)
                ( ELet
                    ()
                    (UniqueDefinition "a" 2)
                    (nat 456)
                    (EVar () (UniqueDefinition "a" 2))
                )
        resolveExprDeps expr `shouldBe` expected

    describe "Typecheck" $ do
      it "Typechecks Prelude successfully" $ do
        case parseModuleAndFormatError preludeInput of
          Right moduleParts -> do
            case moduleFromModuleParts mempty moduleParts of
              Left e -> error (show e)
              Right myModule -> do
                let modules = M.singleton (ModuleHash "123") myModule
                typecheckAllModules modules "" myModule `shouldSatisfy` isRight
          Left e -> error (show e)
