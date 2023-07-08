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
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Modules.Typecheck
import Smol.Core.Modules.Types hiding (Entity (..))
import Smol.Core.Modules.Types.ModuleError
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

findResult :: String -> Either (ModuleError ann) (Module dep (Type dep ann)) -> Expr dep (Type dep ann)
findResult depName = \case
  Right (Module {moExpressions}) ->
    case M.lookup (fromString depName) moExpressions of
      Just a -> tleExpr a
      Nothing -> error "not found in result"
  _ -> error "typecheck failed"

testTypecheck ::
  Text ->
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

testModuleTypecheck ::
  String ->
  Either
    (ModuleError Annotation)
    (Module ResolvedDep (Type ResolvedDep Annotation))
testModuleTypecheck moduleName =
  case testTypecheck (getModuleInput moduleName) of
    Left e -> Left (showModuleError e)
    Right a -> pure a

showModuleError :: ModuleError Annotation -> ModuleError Annotation
showModuleError modErr = unsafePerformIO $ do
  printDiagnostic stdout True True 2 defaultStyle (moduleErrorDiagnostic modErr)
  pure modErr

spec :: Spec
spec = do
  describe "Modules" $ do
    describe "Tests" $ do
      it "Accepts a unit test with type `Boolean`" $ do
        testTypecheck
          ( joinText
              [ "test \"it's fine\" using yes",
                "def yes = True"
              ]
          )
          `shouldSatisfy` isRight

      it "Does not accept a unit test with another type" $ do
        let input =
              joinText
                [ "test \"it's fine\" using yes",
                  "def yes = 100"
                ]
        testTypecheck input
          `shouldSatisfy` \case
            Left
              ( ErrorInTest
                  "it's fine"
                  ( TestDoesNotTypecheck
                      _
                      "yes"
                      (TCTypeMismatch _ _)
                    )
                ) -> True
            _ -> False

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
