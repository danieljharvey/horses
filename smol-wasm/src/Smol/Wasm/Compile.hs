{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Smol.Wasm.Compile where

-- this stays in the main compiler for now because the tests use the
-- typechecker
-- once those are moved, it can go into `backends`

import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Natural
import qualified Language.Wasm.Structure as Wasm
import Smol.Core

type WasmModule = Wasm.Module

newtype WasmM dep a = WasmM
  { getWasmM ::
      StateT (WasmState dep) (Except (WasmError dep)) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState (WasmState dep),
      MonadError (WasmError dep)
    )

data WasmState dep = WasmState
  { wsEnv :: Map (dep Identifier) (Natural, Wasm.ValueType),
    wsCounter :: Natural,
    wsFuncs :: Map Int (WasmFunction dep)
  }

data WasmFunction dep = WasmFunction
  { wfRetType :: Type dep (),
    wfArgs :: [Type dep ()],
    wfBody :: Expr dep (Type dep ())
  }

newtype WasmError dep
  = CouldNotFindVar (dep Identifier)

deriving newtype instance
  (Show (dep Identifier)) =>
  Show (WasmError dep)

emptyState :: (Ord (dep Identifier)) => WasmState dep
emptyState = WasmState mempty 0 mempty

addEnvItem ::
  (Ord (dep Identifier)) =>
  dep Identifier ->
  Wasm.ValueType ->
  WasmM dep Natural
addEnvItem dep wasmType =
  state
    ( \(WasmState env count funcs) ->
        let newCount = count + 1
         in ( count,
              WasmState (env <> M.singleton dep (count, wasmType)) newCount funcs
            )
    )

lookupEnvItem ::
  (Ord (dep Identifier)) =>
  dep Identifier ->
  WasmM dep (Natural, Wasm.ValueType)
lookupEnvItem ident = do
  maybeVal <- gets (\(WasmState env _ _) -> M.lookup ident env)
  case maybeVal of
    Just a -> pure a
    Nothing -> throwError (CouldNotFindVar ident)

runWasmM ::
  (Ord (dep Identifier)) =>
  WasmM dep a ->
  Either (WasmError dep) (a, WasmState dep)
runWasmM (WasmM comp) = runExcept $ runStateT comp emptyState

compileRaw ::
  forall dep.
  ( Ord (dep Identifier),
    Show (dep Identifier),
    Show (dep TypeName),
    Show (dep Identifier),
    Show (dep Constructor),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Expr dep (Type dep ()) ->
  WasmModule
compileRaw expr =
  let func = compileTestFunc expr
      localTypes = []
      funcType = Wasm.FuncType localTypes [Wasm.I32] -- assumption - return is an I32
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

localTypesFromState :: WasmState dep -> [Wasm.ValueType]
localTypesFromState =
  fmap snd . M.elems . wsEnv

compileTestFunc ::
  forall dep ann.
  ( Show ann,
    Show (dep Identifier),
    Show (dep Constructor),
    Show (dep TypeName),
    Ord (dep Identifier),
    Printer (dep Constructor),
    Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Expr dep ann ->
  Wasm.Function
compileTestFunc expr =
  case runWasmM (mainFn emptyState expr) of
    Right (body, wsState) ->
      let locals = localTypesFromState wsState
       in Wasm.Function 0 locals body
    Left e -> error (show e)
  where
    mainFn :: WasmState dep -> Expr dep ann -> WasmM dep [Wasm.Instruction Natural]
    mainFn ws exp' = case exp' of
      (EPrim _ (PInt i)) ->
        pure [Wasm.I32Const (fromIntegral i)]
      (EPrim _ (PBool True)) ->
        pure [Wasm.I32Const 1]
      (EPrim _ (PBool False)) ->
        pure [Wasm.I32Const 0]
      (EIf _ predExpr thenExpr elseExpr) -> do
        let block = Wasm.Inline (Just Wasm.I32) -- return type
        predW <- mainFn ws predExpr
        ifW <-
          Wasm.If
            block
            <$> mainFn ws thenExpr
            <*> mainFn ws elseExpr
        pure $ predW <> [ifW]
      (EInfix _ op a b) -> do
        valA <- mainFn ws a
        valB <- mainFn ws b
        pure $ valA <> valB <> [compileBinOp op]
      (ELet _ ident letExpr body') -> do
        index <- addEnvItem ident Wasm.I32
        letW <- mainFn ws letExpr
        let setW = [Wasm.SetLocal index]
        bodyW <- mainFn ws body'
        pure $ letW <> setW <> bodyW
      (EVar _ ident) -> do
        -- ignoring namespaces
        -- should think about that at some point
        (n, _) <- lookupEnvItem ident
        pure [Wasm.GetLocal n]
      (EApp _ (EVar _ f) a) -> do
        (fIndex, _) <- lookupEnvItem f
        fA <- mainFn ws a
        pure $ fA <> [Wasm.Call fIndex]
      other -> error (show other)

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
