module Language.Mimsa.Backend.Wasm.Compile where

import Language.Mimsa.Types.AST
import qualified Language.Wasm.Structure as Wasm

type WasmModule = Wasm.Module

compile :: Expr var ann -> WasmModule
compile = undefined
