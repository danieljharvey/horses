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
import Smol.Core.Modules.Typecheck
import Smol.Core.Types.Module.ModuleHash
import Test.Hspec
import Smol.Core.Modules.ResolveDeps

-- these are saved in a file that is included in compilation
testInputs :: [(FilePath, Text)]
testInputs =
  fmap (second T.decodeUtf8) $(makeRelativeToProject "test/static/" >>= embedDir)

preludeInput :: Text
preludeInput = case find (\(filename, _) -> "Prelude" `isInfixOf` filename) testInputs of
  Just (_, code) -> code
  Nothing -> error "could not find prelude code"

unsafeParseExpr :: Text -> Expr ParseDep Annotation
unsafeParseExpr input = case parseExprAndFormatError input of
                          Right a -> a
                          Left e -> error (show e)

spec :: Spec
spec = do
  fdescribe "Modules" $ do
    describe "ResolvedDeps" $ do
      it "No deps, marks uniques" $ do
        let expr = unsafeParseExpr "let a = 123 in a"
            expected = undefined
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
