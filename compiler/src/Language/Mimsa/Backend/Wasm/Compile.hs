{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Backend.Wasm.Compile where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Natural
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import qualified Language.Wasm.Structure as Wasm

type WasmModule = Wasm.Module

data WasmState var = WasmState
  { wsEnv :: Map var Natural,
    wsCounter :: Natural
  }

emptyState :: (Ord var) => WasmState var
emptyState = WasmState mempty 0

addEnvItem :: (Ord var) => var -> WasmState var -> (WasmState var, Natural)
addEnvItem var (WasmState env count) =
  let newCount = count + 1
   in ( WasmState (env <> M.singleton var count) newCount,
        count
      )

-- newtype WasmM var a = WasmM {getWasmM :: State (WasmState var) a}

compileRaw ::
  forall var ann.
  (Ord var, Printer (Expr var ann)) =>
  Expr var ann ->
  WasmModule
compileRaw expr =
  let func = compileTestFunc expr
      localTypes = []
      funcType = Wasm.FuncType localTypes [Wasm.I32]
      export =
        Wasm.Export "test" (Wasm.ExportFunc 0)
   in Wasm.Module
        { Wasm.types = [funcType],
          Wasm.functions = [func],
          Wasm.tables = mempty,
          Wasm.mems = mempty,
          Wasm.globals = mempty,
          Wasm.elems = mempty,
          Wasm.datas = mempty,
          Wasm.start = Nothing,
          Wasm.imports = mempty,
          Wasm.exports = [export]
        }

compileTestFunc ::
  forall var ann.
  (Ord var, Printer (Expr var ann)) =>
  Expr var ann ->
  Wasm.Function
compileTestFunc expr =
  let locals = [Wasm.I32, Wasm.I32]
   in Wasm.Function 0 locals body
  where
    body = mainFn emptyState expr
    mainFn :: WasmState var -> Expr var ann -> [Wasm.Instruction Natural]
    mainFn ws exp' = case exp' of
      (MyLiteral _ (MyInt i)) ->
        [Wasm.I32Const (fromIntegral i)]
      (MyLiteral _ (MyBool True)) ->
        [Wasm.I32Const 1]
      (MyLiteral _ (MyBool False)) ->
        [Wasm.I32Const 0]
      (MyIf _ predExpr thenExpr elseExpr) ->
        let block = Wasm.Inline (Just Wasm.I32) -- return type
         in mainFn ws predExpr
              <> [ Wasm.If
                     block
                     (mainFn ws thenExpr)
                     (mainFn ws elseExpr)
                 ]
      (MyInfix _ op a b) -> do
        let valA = mainFn ws a
            valB = mainFn ws b
        valA <> valB <> [compileBinOp op]
      (MyLet _ (Identifier _ ident) letExpr body') -> do
        let (ws2, index) = addEnvItem ident ws
        mainFn ws letExpr <> [Wasm.SetLocal index] <> mainFn ws2 body'
      (MyVar _ _ ident) -> do
        -- ignoring namespaces
        -- should think about that at some point
        case M.lookup ident (wsEnv ws) of
          Just n -> [Wasm.GetLocal n]
          Nothing -> error "found jack shit in env"
      other -> error (T.unpack (prettyPrint other))

compileBinOp :: Operator -> Wasm.Instruction i
compileBinOp op =
  case op of
    Add -> Wasm.IBinOp Wasm.BS32 Wasm.IAdd
    Subtract -> Wasm.IBinOp Wasm.BS32 Wasm.ISub
    Equals -> Wasm.IRelOp Wasm.BS32 Wasm.IEq
    GreaterThan -> Wasm.IRelOp Wasm.BS32 Wasm.IGtU
    LessThan -> Wasm.IRelOp Wasm.BS32 Wasm.ILtU
    GreaterThanOrEqualTo -> Wasm.IRelOp Wasm.BS32 Wasm.IGeU
    LessThanOrEqualTo -> Wasm.IRelOp Wasm.BS32 Wasm.ILeU
    op' -> error (T.unpack (prettyPrint op'))

{-
      (MyLet _ (Identifier _ ident) letExpr body) -> do
        loc <- Wasm.local (Proxy :: Proxy 'Wasm.I32)
        loc .= mainFn ws letExpr
        mainFn (addEnvItem ident loc ws) body
      (MyVar _ _ ident) -> do
        -- ignoring namespaces
        -- should think about that at some point
        case M.lookup ident (wsEnv ws) of
          Just n -> Wasm.ret n
          Nothing -> error "found jack shit in env"
      (MyApp _ f a) -> do
        Wasm.call (mainFn ws f) [mainFn ws a]
-}
