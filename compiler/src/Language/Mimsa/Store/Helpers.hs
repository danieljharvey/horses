{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Store.Helpers where

import Control.Monad.Except
import qualified Data.Map as M
import Language.Mimsa.Types.Error.StoreError
import Language.Mimsa.Types.Store

lookupExprHash ::
  (MonadError StoreError m) =>
  Store ann ->
  ExprHash ->
  m (StoreExpression ann)
lookupExprHash store exprHash =
  case M.lookup exprHash (getStore store) of
    Just se -> pure se
    _ -> throwError (CouldNotFindStoreExpression exprHash)
