{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.Wasm
  ( spec,
  )
where

import Language.Mimsa.Backend.Wasm.Compile
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
    Left _ -> error "invalid module"

spec :: Spec
spec = do
  fdescribe "Wasm" $ do
    describe "Number literals" $ do
      it "int literal 1" $ do
        result <- runWasm (compile (unsafeParseExpr "1"))
        result `shouldBe` Just [Wasm.VI32 1]
      it "int literal 42" $ do
        result <- runWasm (compile (unsafeParseExpr "42"))
        result `shouldBe` Just [Wasm.VI32 42]
    describe "Infix ops" $ do
      it "1 + 1 == 2" $ do
        result <- runWasm (compile (unsafeParseExpr "1 + 1"))
        result `shouldBe` Just [Wasm.VI32 2]
      it "10 - 9" $ do
        result <- runWasm (compile (unsafeParseExpr "10 - 9"))
        result `shouldBe` Just [Wasm.VI32 1]
      it "1 == 1" $ do
        result <- runWasm (compile (unsafeParseExpr "1 == 1"))
        result `shouldBe` Just [Wasm.VI32 1]
      it "1 == 2" $ do
        result <- runWasm (compile (unsafeParseExpr "1 == 2"))
        result `shouldBe` Just [Wasm.VI32 0]
      it "1 < 2" $ do
        result <- runWasm (compile (unsafeParseExpr "1 < 2"))
        result `shouldBe` Just [Wasm.VI32 1]
      it "1 > 2" $ do
        result <- runWasm (compile (unsafeParseExpr "1 > 2"))
        result `shouldBe` Just [Wasm.VI32 0]
      it "1 >= 1" $ do
        result <- runWasm (compile (unsafeParseExpr "1 >= 1"))
        result `shouldBe` Just [Wasm.VI32 1]
      it "1 <= 1" $ do
        result <- runWasm (compile (unsafeParseExpr "1 <= 1"))
        result `shouldBe` Just [Wasm.VI32 1]
      it "1 + 2 + 3 + 4 + 5" $ do
        result <- runWasm (compile (unsafeParseExpr "1 + 2 + 3 + 4 + 5"))
        result `shouldBe` Just [Wasm.VI32 15]
