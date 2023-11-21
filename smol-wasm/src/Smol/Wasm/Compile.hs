{-# LANGUAGE DataKinds #-}
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

import Control.Monad.Except
import Control.Monad.State
import Data.Functor (($>))
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

data WasmExprState dep = WasmExprState
  { _wesCounter :: Natural,
    _wesFuncs :: Map Natural (WasmFunction dep)
  }

data WasmFunction dep = WasmFunction
  { wfRetType :: Type dep (),
    wfArgs :: [(dep Identifier, Type dep ())],
    wfExpr :: WasmExpr dep
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

addWasmFunc ::
  (Ord (dep Identifier), MonadState (WasmExprState dep) m) =>
  WasmFunction dep ->
  m Natural
addWasmFunc wasmFn =
  state
    ( \(WasmExprState count funcs) ->
        let newCount = count + 1
         in ( count,
              WasmExprState newCount ((M.singleton newCount wasmFn) <> funcs)
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
  let (wasmExpr, WasmExprState _ funcs) = runState (toWasmExpr expr) (WasmExprState 0 mempty)
      localTypes = []
      funcType = Wasm.FuncType localTypes [Wasm.I32] -- assumption - return is an I32
      export =
        Wasm.Export "test" (Wasm.ExportFunc 0)
      functions =  [toWasmIR wasmExpr] <> (toWasmIR . wfExpr <$> M.elems funcs)
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

-- simplified AST with functions lifted out
data WasmExpr dep
  = WPrim Prim
  | WIf (WasmExpr dep) (WasmExpr dep) (WasmExpr dep)
  | WInfix Op (WasmExpr dep) (WasmExpr dep)
  | WLet (dep Identifier) (WasmExpr dep) (WasmExpr dep)
  | WVar (dep Identifier)
  | WApp (WasmExpr dep) (WasmExpr dep)
  | WFnRef Natural

toWasmExpr ::
  (MonadState (WasmExprState dep) m, Ord (dep Identifier), Show ann) =>
  Expr dep (Type dep ann) ->
  m (WasmExpr dep)
toWasmExpr
  (EPrim _ prim) = pure $ WPrim prim
toWasmExpr (EIf _ predExpr thenExpr elseExpr) =
  WIf <$> toWasmExpr predExpr <*> toWasmExpr thenExpr <*> toWasmExpr elseExpr
toWasmExpr (EInfix _ op a b) =
  WInfix op <$> toWasmExpr a <*> toWasmExpr b
toWasmExpr (ELet _ ident letExpr body') =
  WLet ident <$> toWasmExpr letExpr <*> toWasmExpr body'
toWasmExpr (EVar _ ident) =
  pure (WVar ident)
toWasmExpr (EApp _ fn arg) =
  WApp <$> toWasmExpr fn <*> toWasmExpr arg
toWasmExpr (EAnn _ _ inner) =
  toWasmExpr inner
toWasmExpr (ELambda ty ident body) = do
  wasmBody <- toWasmExpr body
  case ty of
    (TFunc _ _ tyArg tyRet) -> do
      nat <-
        addWasmFunc
          ( WasmFunction
              { wfRetType = tyRet $> (),
                wfArgs = [(ident, tyArg $> ())],
                wfExpr = wasmBody
              }
          )
      pure (WFnRef nat)
    _ -> error "should be function type"
toWasmExpr _ = error "Unimplemented toWasmExpr"

toWasmIR ::
  forall dep.
  (Ord (dep Identifier), Show (dep Identifier)) =>
  WasmExpr dep ->
  Wasm.Function
toWasmIR expr =
  case runWasmM (mainFn emptyState expr) of
    Right (body, wsState) ->
      let locals = localTypesFromState wsState
       in Wasm.Function 0 locals body
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
        -- ignoring namespaces
        -- should think about that at some point
        (n, _) <- lookupEnvItem ident
        pure [Wasm.GetLocal n]
      (WApp (WVar f) a) -> do
        (fIndex, _) <- lookupEnvItem f
        fA <- mainFn ws a
        pure $ fA <> [Wasm.Call fIndex]
      _ -> error "toWasmIR other"

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
