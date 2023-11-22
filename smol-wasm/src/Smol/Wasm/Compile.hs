{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Smol.Wasm.Compile (compileRaw, WasmFunction (..)) where

-- this stays in the main compiler for now because the tests use the
-- typechecker
-- once those are moved, it can go into `backends`
import qualified Data.List.NonEmpty as NE
import Smol.Wasm.FromExpr
import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Natural
import qualified Language.Wasm.Structure as Wasm
import Smol.Core

type WasmModule = Wasm.Module

newtype WasmM dep a = WasmM
  { _getWasmM ::
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
    _wsCounter :: Natural
  }

newtype WasmError dep
  = CouldNotFindVar (dep Identifier)

deriving newtype instance
  (Show (dep Identifier)) =>
  Show (WasmError dep)

emptyState :: (Ord (dep Identifier)) => WasmState dep
emptyState = WasmState mempty 0

addEnvItem ::
  (Ord (dep Identifier), MonadState (WasmState dep) m) =>
  dep Identifier ->
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
  (Ord (dep Identifier)) =>
  dep Identifier ->
  WasmM dep (Natural, Wasm.ValueType)
lookupEnvItem ident = do
  maybeVal <- gets (\(WasmState env _) -> M.lookup ident env)
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
  let funcs = toWasmFunc expr
      localTypes = []
      funcType = Wasm.FuncType localTypes [Wasm.I32] -- assumption - return is an I32
      export =
        Wasm.Export "test" (Wasm.ExportFunc 0)
      functions =  toWasmIR <$> NE.toList funcs
   in Wasm.Module
        { Wasm.types = [funcType],
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

localTypesFromState :: WasmState dep -> [Wasm.ValueType]
localTypesFromState =
  fmap snd . M.elems . wsEnv

toWasmIR ::
  forall dep.
  (Ord (dep Identifier), Show (dep Identifier)) =>
  WasmFunction dep ->
  Wasm.Function
toWasmIR wasmFunc =
  let action = do
                 _ <- traverse (\(ident, _t) -> addEnvItem ident Wasm.I32) (wfArgs wasmFunc)
                 mainFn emptyState (wfExpr wasmFunc)
   in case runWasmM action of
    Right (body, wsState) ->
      let locals = localTypesFromState wsState
       in Wasm.Function (wfIdentifier wasmFunc) locals body
    Left e -> error (show e)
  where
    mainFn :: WasmState dep -> WasmExpr dep -> WasmM dep [Wasm.Instruction Natural]
    mainFn ws exp' = case exp' of
      (WPrim (PInt i)) ->
        pure [Wasm.I32Const (fromIntegral i)]
      (WPrim (PBool True)) ->
        pure [Wasm.I32Const 1]
      (WPrim (PBool False)) ->
        pure [Wasm.I32Const 0]
      (WPrim _) -> error "toWasmIR prim"
      (WIf predExpr thenExpr elseExpr) -> do
        let block = Wasm.Inline (Just Wasm.I32) -- return type
        predW <- mainFn ws predExpr
        ifW <-
          Wasm.If
            block
            <$> mainFn ws thenExpr
            <*> mainFn ws elseExpr
        pure $ predW <> [ifW]
      (WInfix op a b) -> do
        valA <- mainFn ws a
        valB <- mainFn ws b
        pure $ valA <> valB <> [compileBinOp op]
      (WLet ident letExpr body') -> do
        index <- addEnvItem ident Wasm.I32
        letW <- mainFn ws letExpr
        let setW = [Wasm.SetLocal index]
        bodyW <- mainFn ws body'
        pure $ letW <> setW <> bodyW
      (WVar ident) -> do
        (n, _) <- lookupEnvItem ident
        pure [Wasm.GetLocal n]
      (WApp (WVar f) a) -> do
        (fIndex, _) <- lookupEnvItem f
        fA <- mainFn ws a
        pure $ fA <> [Wasm.Call fIndex]
      (WApp (WFnRef nat) a) -> do
        fA <- mainFn ws a
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
