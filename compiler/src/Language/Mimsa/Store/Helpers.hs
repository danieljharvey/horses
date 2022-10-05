{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Store.Helpers (lookupExprHashInStore, setStoreExpression) where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Language.Mimsa.Types.AST.Expr
import Language.Mimsa.Types.Error.StoreError
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

lookupExprHashInStore ::
  (MonadError StoreError m) =>
  Store ann ->
  ExprHash ->
  m (StoreExpression ann)
lookupExprHashInStore store exprHash =
  case M.lookup exprHash (getStore store) of
    Just se -> pure se
    _ -> throwError (CouldNotFindStoreExpression exprHash)

setStoreExpression :: StoreExpression ann -> Expr Name annB -> StoreExpression annB
setStoreExpression se@StoreExpression {} expr =
  se {seExpr = expr}
setStoreExpression (StoreDataType a b) _ = StoreDataType a b
