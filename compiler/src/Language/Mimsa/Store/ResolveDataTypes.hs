{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Store.ResolveDataTypes (resolveDataTypes, createTypeMap, storeExprToDataTypes) where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Store.ExtractTypes
import Language.Mimsa.Store.Helpers
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Modules.ModuleName
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
      (lookupExprHash store')
      (M.elems (getTypeBindings $ storeTypeBindings storeExpr))
  pure (createTypeMap exprs)

createTypeMap :: [StoreExpression ann] -> Map (Maybe ModuleName, TypeName) DataType
createTypeMap dataTypes =
  mconcat (storeExprToDataTypes <$> dataTypes)

storeExprToDataTypes :: StoreExpression ann -> Map (Maybe ModuleName, TypeName) DataType
storeExprToDataTypes =
  mconcat
    . fmap withDt
    . S.toList
    . extractDataTypes
    . storeExpression
  where
    withDt dt@(DataType tyName _ _) =
      M.singleton (Nothing, tyName) dt
