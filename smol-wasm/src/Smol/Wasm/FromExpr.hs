{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Smol.Wasm.FromExpr (toWasmFunc, WasmExpr(..), WasmFunction(..) ) where

import qualified Data.List.NonEmpty as NE
import Control.Monad.State
import Data.Functor (($>), void)
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Natural
import Smol.Core

data WasmExprState dep = WasmExprState
  { _wesCounter :: Natural,
    _wesFuncs :: Map Natural (WasmFunction dep)
  }

data WasmFunction dep = WasmFunction
  { wfRetType :: Type dep (),
    wfArgs :: [(dep Identifier, Type dep ())],
    wfExpr :: WasmExpr dep,
    wfIdentifier :: Natural
  }

addWasmFunc ::
  (Ord (dep Identifier), MonadState (WasmExprState dep) m) =>
  WasmFunction dep ->
  m Natural
addWasmFunc wasmFn =
  state
    ( \(WasmExprState count funcs) ->
        let newCount = count + 1
            wasmFuncWithIdentifier = wasmFn { wfIdentifier = count }
         in ( count,
              WasmExprState newCount ((M.singleton newCount wasmFuncWithIdentifier) <> funcs)
            )
    )

-- simplified AST with functions lifted out
data WasmExpr dep
  = WPrim Prim
  | WIf (WasmExpr dep) (WasmExpr dep) (WasmExpr dep)
  | WInfix Op (WasmExpr dep) (WasmExpr dep)
  | WLet (dep Identifier) (WasmExpr dep) (WasmExpr dep)
  | WVar (dep Identifier)
  | WApp (WasmExpr dep) (WasmExpr dep)
  | WFnRef Natural

-- turn onto expr into one or more functions
-- the first one will be `main`
toWasmFunc :: (Show ann, Ord (dep Identifier)) => Expr dep (Type dep ann) -> NE.NonEmpty (WasmFunction dep)
toWasmFunc expr =
  let (wasmExpr, WasmExprState _ funcs) = runState (toWasmExpr expr) (WasmExprState 0 mempty)
      in (WasmFunction {  wfRetType = void (getExprAnnotation expr),
                                    wfArgs = mempty, -- assume this is `main` for now, although it might not be
                            wfExpr = wasmExpr,
                            wfIdentifier = 0
                                 }) NE.:| M.elems funcs

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
                wfExpr = wasmBody,
                wfIdentifier = 0 -- we attach the correct number in `addWasmFunc`
              }
          )
      pure (WFnRef nat)
    _ -> error "should be function type"
toWasmExpr _ = error "Unimplemented toWasmExpr"
