{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Store.ResolveDataTypes (resolveDataTypes, createTypeMap, storeExprToDataTypes) where

import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Language.Mimsa.Store.Helpers
import Language.Mimsa.Core
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Store

-- given a StoreExpression (and the Store), return all DataTypes used in the
-- expression
resolveDataTypes ::
  (MonadError StoreError m) =>
  Store ann ->
  StoreExpression ann ->
  m (Map (Maybe ModuleName, TypeName) DataType)
resolveDataTypes store' storeExpr = do
  exprs <-
    traverse
      (lookupExprHashInStore store')
      (M.elems (storeTypeBindings storeExpr))
  pure (createTypeMap exprs)

createTypeMap :: [StoreExpression ann] -> Map (Maybe ModuleName, TypeName) DataType
createTypeMap dataTypes =
  mconcat (storeExprToDataTypes <$> dataTypes)

storeExprToDataTypes :: StoreExpression ann -> Map (Maybe ModuleName, TypeName) DataType
storeExprToDataTypes (StoreDataType dt@(DataType tn _ _) _) =
  M.singleton (Nothing, tn) dt
storeExprToDataTypes _ = mempty
