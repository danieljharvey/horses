{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.Wasm
  ( spec,
  )
where

import Data.Bifunctor
import Data.Text (Text)
import Language.Mimsa.Backend.Wasm.Compile
import Language.Mimsa.Typechecker.NumberVars
import Language.Mimsa.Typechecker.Typecheck
import Language.Mimsa.Core
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import qualified Language.Wasm as Wasm
import qualified Language.Wasm.Interpreter as Wasm
import Test.Hspec
import Test.Utils.Helpers

runWasm :: Wasm.Module -> IO (Maybe [Wasm.Value])
runWasm wasmModule = do
  case Wasm.validate wasmModule of
    Right validModule -> do
      (result, store) <- Wasm.instantiate Wasm.emptyStore mempty validModule
      case result of
        Right moduleInstance ->
          Wasm.invokeExport store moduleInstance "test" mempty
        Left e -> error e
    Left e -> do
      print wasmModule
      error $ "invalid module: " <> show e

typecheck' ::
  (Monoid ann) =>
  Expr Name Annotation ->
  Expr Name (Type ann)
typecheck' expr = do
  let numberedExpr = fromRight (addNumbersToStoreExpression expr mempty)
  let result =
        fmap (\(_, _, a, _) -> first fst a)
          . typecheck mempty mempty
          $ numberedExpr
  (fmap . fmap) (const mempty) (fromRight result)

wasmTest :: Text -> IO (Maybe [Wasm.Value])
wasmTest input =
  let expr = typecheck' $ unsafeParseExpr' input
   in runWasm (compileRaw expr)

spec :: Spec
spec = do
  describe "Wasm" $ do
    describe "Number literals" $ do
      it "int literal 1" $ do
        result <- wasmTest "1"
        result `shouldBe` Just [Wasm.VI32 1]
      it "int literal 42" $ do
        result <- wasmTest "42"
        result `shouldBe` Just [Wasm.VI32 42]
    describe "Boolean literals" $ do
      it "true" $ do
        result <- wasmTest "True"
        result `shouldBe` Just [Wasm.VI32 1]
      it "false" $ do
        result <- wasmTest "False"
        result `shouldBe` Just [Wasm.VI32 0]
    describe "If expression" $ do
      it "true branch" $ do
        result <- wasmTest "if True then 42 else 5"
        result `shouldBe` Just [Wasm.VI32 42]
      it "false branch" $ do
        result <- wasmTest "if False then 42 else 5"
        result `shouldBe` Just [Wasm.VI32 5]
      it "using infix op" $ do
        result <- wasmTest "if 4 == 5 then 42 else 5"
        result `shouldBe` Just [Wasm.VI32 5]
    describe "Infix ops" $ do
      it "1 + 1 == 2" $ do
        result <- wasmTest "1 + 1"
        result `shouldBe` Just [Wasm.VI32 2]
      it "10 - 9" $ do
        result <- wasmTest "10 - 9"
        result `shouldBe` Just [Wasm.VI32 1]
      it "1 == 1" $ do
        result <- wasmTest "1 == 1"
        result `shouldBe` Just [Wasm.VI32 1]
      it "1 == 2" $ do
        result <- wasmTest "1 == 2"
        result `shouldBe` Just [Wasm.VI32 0]
      it "1 < 2" $ do
        result <- wasmTest "1 < 2"
        result `shouldBe` Just [Wasm.VI32 1]
      it "1 > 2" $ do
        result <- wasmTest "1 > 2"
        result `shouldBe` Just [Wasm.VI32 0]
      it "1 >= 1" $ do
        result <- wasmTest "1 >= 1"
        result `shouldBe` Just [Wasm.VI32 1]
      it "1 <= 1" $ do
        result <- wasmTest "1 <= 1"
        result `shouldBe` Just [Wasm.VI32 1]
      it "1 + 2 + 3 + 4 + 5" $ do
        result <- wasmTest "1 + 2 + 3 + 4 + 5"
        result `shouldBe` Just [Wasm.VI32 15]
    describe "Function" $ do
      xit "let inc = \\a -> a + 1; inc 1" $ do
        result <- wasmTest "let inc = \\a -> a + 1; inc 1"
        result `shouldBe` Just [Wasm.VI32 2]
    describe "Variables" $ do
      it "let a = 1 in a + 1" $ do
        result <- wasmTest "let a = 1 in a + 1"
        result `shouldBe` Just [Wasm.VI32 2]
      it "let a = 1; let b = 2; a + b" $ do
        result <- wasmTest "let a = 1; let b = 2; a + b"
        result `shouldBe` Just [Wasm.VI32 3]
      it "let a = 1; let b = 2; let c = 3; a + b - c" $ do
        result <- wasmTest "let a = 1; let b = 2; let c = 3; a + b - c"
        result `shouldBe` Just [Wasm.VI32 0]
