{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Smol.Wasm.Compile (compileRaw, WasmFunction (..)) where

-- this stays in the main compiler for now because the tests use the
-- typechecker
-- once those are moved, it can go into `backends`

import Control.Monad.Except
import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace
import GHC.Natural
import qualified Language.Wasm.Structure as Wasm
import Smol.Core
import Smol.Wasm.FromExpr

type WasmModule = Wasm.Module

newtype WasmM a = WasmM
  { _getWasmM ::
      StateT WasmState (Except WasmError) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState WasmState,
      MonadError WasmError
    )

data WasmState = WasmState
  { wsEnv :: Map (WasmDep Identifier) (Natural, Wasm.ValueType),
    wsCounter :: Natural
  }

newtype WasmError
  = CouldNotFindVar (WasmDep Identifier)
  deriving newtype (Eq, Ord, Show)

emptyState :: Natural -> WasmState
emptyState = WasmState mempty

addEnvItem ::
  (MonadState WasmState m) =>
  WasmDep Identifier ->
  Wasm.ValueType ->
  m Natural
addEnvItem dep wasmType =
  state
    ( \(WasmState env count) ->
        let newCount = count + 1
         in ( count,
              WasmState (env <> M.singleton dep (count, wasmType)) newCount
            )
    )

lookupEnvItem ::
  WasmDep Identifier ->
  WasmM (Natural, Wasm.ValueType)
lookupEnvItem ident = do
  maybeVal <- gets (\(WasmState env _) -> M.lookup ident env)
  case maybeVal of
    Just a -> pure a
    Nothing -> throwError (CouldNotFindVar ident)

runWasmM ::
  Natural ->
  WasmM a ->
  Either WasmError (a, WasmState)
runWasmM startingCount (WasmM comp) = runExcept $ runStateT comp (emptyState startingCount)

compileRaw ::
  Expr WasmDep (Type WasmDep ()) ->
  WasmModule
compileRaw expr =
  let funcs = toWasmFunc expr
      export =
        Wasm.Export "test" (Wasm.ExportFunc 0)

      functions = (\(_,_,a) -> a) . functionToWasmIR 0 <$> NE.toList funcs
      funcTypes = toFuncType <$> NE.toList funcs
   in Wasm.Module
        { Wasm.types = funcTypes,
          Wasm.functions = functions,
          Wasm.tables = mempty,
          Wasm.mems = mempty,
          Wasm.globals = mempty,
          Wasm.elems = mempty,
          Wasm.datas = mempty,
          Wasm.start = Nothing,
          Wasm.imports = mempty,
          Wasm.exports = [export]
        }

toFuncType :: WasmFunction -> Wasm.FuncType
toFuncType (WasmFunction {wfArgs, wfRetType}) =
  Wasm.FuncType (typeToIR . snd <$> wfArgs) [typeToIR wfRetType]

typeToIR :: (Show ann) => Type WasmDep ann -> Wasm.ValueType
typeToIR (TPrim _ TPInt) = Wasm.I32
typeToIR (TPrim _ TPBool) = Wasm.I32
typeToIR (TLiteral _ (TLInt {})) = Wasm.I32
typeToIR (TLiteral _ (TLBool {})) = Wasm.I32
typeToIR other = error (show other)

localTypesFromState :: WasmState -> [Wasm.ValueType]
localTypesFromState =
  fmap snd . M.elems . wsEnv

functionToWasmIR ::
  Natural ->
  WasmFunction ->
    (Natural,[Wasm.ValueType],Wasm.Function)
functionToWasmIR offset wasmFunc =
  let action = do
        _ <- traverse (\(ident, _t) -> addEnvItem ident Wasm.I32) (wfArgs (traceShowId wasmFunc))
        toWasmIR (wfExpr wasmFunc)
   in case runWasmM offset action of
        Right (body, wsState) ->
          let locals = traceShowId $ localTypesFromState wsState
           in (wsCounter wsState, locals, Wasm.Function (wfIdentifier wasmFunc) locals body)
        Left e -> error (show e)

toWasmIR :: WasmExpr -> WasmM [Wasm.Instruction Natural]
toWasmIR exp' = case exp' of
  (WPrim (PInt i)) ->
    pure [Wasm.I32Const (fromIntegral i)]
  (WPrim (PBool True)) ->
    pure [Wasm.I32Const 1]
  (WPrim (PBool False)) ->
    pure [Wasm.I32Const 0]
  (WPrim _) -> error "toWasmIR prim"
  (WIf predExpr thenExpr elseExpr) -> do
    let block = Wasm.Inline (Just Wasm.I32) -- return type
    predW <- toWasmIR predExpr
    ifW <-
      Wasm.If
        block
        <$> toWasmIR thenExpr
        <*> toWasmIR elseExpr
    pure $ predW <> [ifW]
  (WInfix op a b) -> do
    valA <- toWasmIR a
    valB <- toWasmIR b
    pure $ valA <> valB <> [compileBinOp op]
  (WLet ident letExpr body') -> do
    index <- addEnvItem ident Wasm.I32
    letW <- toWasmIR letExpr
    let setW = [Wasm.SetLocal index]
    bodyW <- toWasmIR body'
    pure $ letW <> setW <> bodyW
  (WVar ident) -> do
    (n, _) <- lookupEnvItem ident
    pure [Wasm.GetLocal n]
  (WApp (WVar f) a) -> do
    (fIndex, _) <- lookupEnvItem f
    fA <- toWasmIR a
    pure $ fA <> [Wasm.Call fIndex]
  (WApp (WFnRef nat) a) -> do
    fA <- toWasmIR a
    pure $ fA <> [Wasm.Call nat]
  (WFnRef n) ->
    -- the number is a WASM variable name, sort of
    pure [Wasm.GetLocal n]
  (WApp _ _) -> error "Wapp other"

compileBinOp :: Op -> Wasm.Instruction i
compileBinOp op =
  case op of
    OpAdd -> Wasm.IBinOp Wasm.BS32 Wasm.IAdd
    -- Subtract -> Wasm.IBinOp Wasm.BS32 Wasm.ISub
    OpEquals -> Wasm.IRelOp Wasm.BS32 Wasm.IEq

-- GreaterThan -> Wasm.IRelOp Wasm.BS32 Wasm.IGtU
-- LessThan -> Wasm.IRelOp Wasm.BS32 Wasm.ILtU
-- GreaterThanOrEqualTo -> Wasm.IRelOp Wasm.BS32 Wasm.IGeU
-- LessThanOrEqualTo -> Wasm.IRelOp Wasm.BS32 Wasm.ILeU
