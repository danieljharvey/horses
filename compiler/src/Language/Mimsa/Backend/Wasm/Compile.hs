{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Wasm.Compile where

import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import qualified Language.Wasm.Builder as Wasm
import qualified Language.Wasm.Structure as Wasm

type WasmModule = Wasm.Module

compile :: (Printer (Expr var ann)) => Expr var ann -> WasmModule
compile expr =
  Wasm.genMod (Wasm.export "test" (Wasm.fun Wasm.i32 (mainFn expr)))
  where
    mainFn exp' = case exp' of
      (MyLiteral _ (MyInt i)) ->
        Wasm.i32c i
      (MyInfix _ op a b) -> do
        let valA = mainFn a
            valB = mainFn b
        case op of
          Add -> Wasm.add valA valB
          Subtract -> Wasm.sub valA valB
          Equals -> Wasm.eq valA valB
          GreaterThan -> Wasm.gt_s valA valB
          LessThan -> Wasm.lt_s valA valB
          GreaterThanOrEqualTo -> Wasm.ge_s valA valB
          LessThanOrEqualTo -> Wasm.le_s valA valB
          op' -> error (T.unpack (prettyPrint op'))
      other -> error (T.unpack (prettyPrint other))
