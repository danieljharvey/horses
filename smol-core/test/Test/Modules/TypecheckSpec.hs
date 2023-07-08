{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Modules.TypecheckSpec (spec) where

import Data.Bifunctor (second)
import Data.Either (isRight)
import Data.FileEmbed
import Data.Foldable (find)
import Data.Functor (void)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Error.Diagnose (defaultStyle, printDiagnostic, stdout)
import Smol.Core
import Smol.Core.Modules.FromParts
import Smol.Core.Modules.ModuleError
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Types.Module hiding (Entity (..))
import System.IO.Unsafe
import Test.Helpers
import Test.Hspec

-- these are saved in a file that is included in compilation
testInputs :: [(FilePath, Text)]
testInputs =
  fmap (second T.decodeUtf8) $(makeRelativeToProject "test/static/" >>= embedDir)

getModuleInput :: String -> Text
getModuleInput moduleName = case find (\(filename, _) -> moduleName `isInfixOf` filename) testInputs of
  Just (_, code) -> code
  Nothing -> error $ "could not find " <> moduleName <> " code"

findResult :: String -> Either ModuleError (Module dep ann) -> Expr dep ann
findResult depName = \case
  Right (Module {moExpressions}) ->
    case M.lookup (fromString depName) moExpressions of
      Just a -> tleExpr a
      Nothing -> error "not found in result"
  _ -> error "typecheck failed"

testModuleTypecheck :: String -> Either ModuleError (Module ResolvedDep (Type ResolvedDep Annotation))
testModuleTypecheck moduleName =
  case parseModuleAndFormatError (getModuleInput moduleName) of
    Right moduleParts -> do
      case moduleFromModuleParts moduleParts >>= resolveModuleDeps of
        Left e -> error (show e)
        Right (myModule, deps) -> do
          case typecheckModule (getModuleInput moduleName) myModule deps of
            Left e ->
              showModuleError e
                >> Left e
            Right a -> pure a
    Left e -> error (show e)

showModuleError :: ModuleError -> a
showModuleError modErr = unsafePerformIO $ do
  printDiagnostic stdout True True 2 defaultStyle (moduleErrorDiagnostic modErr)
  error "oh no"

spec :: Spec
spec = do
  describe "Modules" $ do
    describe "Tests" $ do
      it "Accepts a unit test with type `Boolean`" $ do
        True `shouldBe` False

      it "Does not accept a unit test with another type" $ do
        True `shouldBe` False

    describe "Typecheck" $ do
      it "Typechecks Prelude successfully" $ do
        testModuleTypecheck "Prelude" `shouldSatisfy` isRight

      it "Typechecks Maybe successfully" $ do
        testModuleTypecheck "Maybe" `shouldSatisfy` isRight

      it "Typechecks Either successfully" $ do
        testModuleTypecheck "Either" `shouldSatisfy` isRight

      it "Typechecks Expr successfully" $ do
        testModuleTypecheck "Expr" `shouldSatisfy` isRight

      -- these are a mess and we need to simplify the types
      xit "Typechecks Globals successfully" $ do
        let result = testModuleTypecheck "Globals"

        void (getExprAnnotation (findResult "noGlobal" result))
          `shouldBe` tyIntLit [100]

        void (getExprAnnotation (findResult "useGlobal" result))
          `shouldBe` TGlobals () (M.singleton "valueA" (tyIntLit [20])) (tyIntLit [20])

        void (getExprAnnotation (findResult "useGlobalIndirectly" result))
          `shouldBe` TGlobals () (M.singleton "valueA" (tyIntLit [20])) tyInt

      it "Typechecks Reader successfully" $ do
        testModuleTypecheck "Reader" `shouldSatisfy` isRight

      it "Typechecks State successfully" $ do
        testModuleTypecheck "State" `shouldSatisfy` isRight
