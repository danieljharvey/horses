{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Backend.Wasm.Compile where

import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Natural
import Language.Mimsa.Core
import qualified Language.Wasm.Structure as Wasm

type WasmModule = Wasm.Module

data WasmState var = WasmState
  { wsEnv :: Map var (Natural, Wasm.ValueType),
    wsCounter :: Natural,
    wsFuncs :: Map Int (WasmFunction var)
  }

data WasmFunction var = WasmFunction
  { wfRetType :: Type (),
    wfArgs :: [Type ()],
    wfBody :: Expr var (Type ())
  }

newtype WasmError var
  = CouldNotFindVar var
  deriving newtype (Show)

emptyState :: (Ord var) => WasmState var
emptyState = WasmState mempty 0 mempty

addEnvItem :: (Ord var) => var -> Wasm.ValueType -> WasmM var Natural
addEnvItem var wasmType =
  state
    ( \(WasmState env count funcs) ->
        let newCount = count + 1
         in ( count,
              WasmState (env <> M.singleton var (count, wasmType)) newCount funcs
            )
    )

lookupEnvItem :: (Ord var) => var -> WasmM var (Natural, Wasm.ValueType)
lookupEnvItem var = do
  maybeVal <- gets (\(WasmState env _ _) -> M.lookup var env)
  case maybeVal of
    Just a -> pure a
    Nothing -> throwError (CouldNotFindVar var)

newtype WasmM var a = WasmM
  { getWasmM ::
      StateT (WasmState var) (Except (WasmError var)) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState (WasmState var),
      MonadError (WasmError var)
    )

runWasmM ::
  (Ord var) =>
  WasmM var a ->
  Either (WasmError var) (a, WasmState var)
runWasmM (WasmM comp) = runExcept $ runStateT comp emptyState

compileRaw ::
  forall var.
  (Ord var, Show var, Printer (Expr var (Type ()))) =>
  Expr var (Type ()) ->
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

localTypesFromState :: WasmState var -> [Wasm.ValueType]
localTypesFromState =
  fmap snd . M.elems . wsEnv

compileTestFunc ::
  forall var ann.
  (Ord var, Show var, Printer (Expr var ann)) =>
  Expr var ann ->
  Wasm.Function
compileTestFunc expr =
  case runWasmM (mainFn emptyState expr) of
    Right (body, wsState) ->
      let locals = localTypesFromState wsState
       in Wasm.Function 0 locals body
    Left e -> error (show e)
  where
    mainFn :: WasmState var -> Expr var ann -> WasmM var [Wasm.Instruction Natural]
    mainFn ws exp' = case exp' of
      (MyLiteral _ (MyInt i)) ->
        pure [Wasm.I32Const (fromIntegral i)]
      (MyLiteral _ (MyBool True)) ->
        pure [Wasm.I32Const 1]
      (MyLiteral _ (MyBool False)) ->
        pure [Wasm.I32Const 0]
      (MyIf _ predExpr thenExpr elseExpr) -> do
        let block = Wasm.Inline (Just Wasm.I32) -- return type
        predW <- mainFn ws predExpr
        ifW <-
          Wasm.If
            block
            <$> mainFn ws thenExpr
            <*> mainFn ws elseExpr
        pure $ predW <> [ifW]
      (MyInfix _ op a b) -> do
        valA <- mainFn ws a
        valB <- mainFn ws b
        pure $ valA <> valB <> [compileBinOp op]
      (MyLet _ (Identifier _ ident) letExpr body') -> do
        index <- addEnvItem ident Wasm.I32
        letW <- mainFn ws letExpr
        let setW = [Wasm.SetLocal index]
        bodyW <- mainFn ws body'
        pure $ letW <> setW <> bodyW
      (MyVar _ _ ident) -> do
        -- ignoring namespaces
        -- should think about that at some point
        (n, _) <- lookupEnvItem ident
        pure [Wasm.GetLocal n]
      (MyApp _ (MyVar _ _ f) a) -> do
        (fIndex, _) <- lookupEnvItem f
        fA <- mainFn ws a
        pure $ fA <> [Wasm.Call fIndex]
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
