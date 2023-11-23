{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Smol.Wasm.FromExpr (toWasmFunc, WasmDep (..), WasmExpr (..), WasmFunction (..)) where

import Control.Monad.State
import Data.Functor (void, ($>))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Natural
import Smol.Core

-- | Use our own Identity shaped thing to avoid passing `dep` around
newtype WasmDep a = WasmDep a
  deriving stock (Eq, Ord, Show)
  deriving newtype (Printer)

-- simplified AST with functions lifted out
data WasmExpr
  = WPrim Prim
  | WIf WasmExpr WasmExpr WasmExpr
  | WInfix Op WasmExpr WasmExpr
  | WLet (WasmDep Identifier) WasmExpr WasmExpr
  | WVar (WasmDep Identifier)
  | WApp WasmExpr WasmExpr
  | WFnRef Natural
  deriving stock (Eq, Ord, Show)

data WasmExprState = WasmExprState
  { _wesCounter :: Natural,
    _wesFuncs :: Map Natural WasmFunction
  }
  deriving stock (Eq, Ord, Show)

data WasmFunction = WasmFunction
  { wfRetType :: Type WasmDep (),
    wfArgs :: [(WasmDep Identifier, Type WasmDep ())],
    wfExpr :: WasmExpr,
    wfIdentifier :: Natural
  }
  deriving stock (Eq, Ord, Show)

addWasmFunc ::
  (MonadState WasmExprState m) =>
  WasmFunction ->
  m Natural
addWasmFunc wasmFn =
  state
    ( \(WasmExprState count funcs) ->
        let newCount = count + 1
            wasmFuncWithIdentifier = wasmFn {wfIdentifier = newCount}
         in ( count,
              WasmExprState newCount ((M.singleton newCount wasmFuncWithIdentifier) <> funcs)
            )
    )

-- turn onto expr into one or more functions
-- the first one will be `main`
toWasmFunc :: (Show ann) => Expr WasmDep (Type WasmDep ann) -> NE.NonEmpty WasmFunction
toWasmFunc expr =
  let (wasmExpr, WasmExprState _ funcs) = runState (toWasmExpr expr) (WasmExprState 0 mempty)
   in ( WasmFunction
          { wfRetType = void (getExprAnnotation expr),
            wfArgs = mempty, -- assume this is `main` for now, although it might not be
            wfExpr = wasmExpr,
            wfIdentifier = 0
          }
      )
        NE.:| M.elems funcs

toWasmExpr ::
  (MonadState WasmExprState m, Show ann) =>
  Expr WasmDep (Type WasmDep ann) ->
  m WasmExpr
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
                wfExpr = wasmBody,
                wfIdentifier = 0 -- we attach the correct number in `addWasmFunc`
              }
          )
      pure (WFnRef nat)
    _ -> error "should be function type"
toWasmExpr _ = error "Unimplemented toWasmExpr"
