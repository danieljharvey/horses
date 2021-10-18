module Language.Mimsa.Store.ResolvedDeps
  ( resolveDeps,
    resolveTypeDeps,
    recursiveResolve,
  )
where

import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Language.Mimsa.Store.ExtractTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

-- we spend so much time passing the whole store around to match hashes
-- lets create one way of resolving a pile of them and be done with it

resolveDeps :: Store ann -> Bindings -> Either StoreError (ResolvedDeps ann)
resolveDeps (Store items) (Bindings bindings') =
  case partitionEithers foundItems of
    ([], found) -> Right (ResolvedDeps (M.fromList found))
    (fails, _) -> Left $ CouldNotFindExprHashForBindings fails
  where
    foundItems =
      ( \(name, hash) -> case M.lookup hash items of
          Just storeExpr' ->
            Right (name, (hash, storeExpr'))
          Nothing -> Left name
      )
        <$> M.toList bindings'

extractDataType ::
  StoreExpression ann ->
  Maybe DataType
extractDataType se =
  let types = extractDataTypes (storeExpression se)
   in listToMaybe . S.toList $ types

resolveTypeDeps ::
  Store ann ->
  TypeBindings ->
  Either StoreError ResolvedTypeDeps
resolveTypeDeps (Store items) (TypeBindings bindings') =
  case partitionEithers foundItems of
    ([], found) -> Right (ResolvedTypeDeps (M.fromList found))
    (fails, _) -> Left $ CouldNotFindExprHashForTypeBindings fails
  where
    foundItems =
      ( \(tyCon, hash) -> case M.lookup hash items of
          Just storeExpr -> do
            case extractDataType storeExpr of
              Just dt -> Right (tyCon, (hash, dt))
              _ -> Left tyCon
          Nothing -> Left tyCon
      )
        <$> M.toList bindings'

resolveTypeStoreExpressions ::
  Store ann ->
  TypeBindings ->
  Either StoreError (Map TyCon (ExprHash, StoreExpression ann))
resolveTypeStoreExpressions (Store items) (TypeBindings bindings') =
  case partitionEithers foundItems of
    ([], found) -> Right (M.fromList found)
    (fails, _) -> Left $ CouldNotFindExprHashForTypeBindings fails
  where
    foundItems =
      ( \(tyCon, hash) -> case M.lookup hash items of
          Just storeExpr ->
            Right (tyCon, (hash, storeExpr))
          Nothing -> Left tyCon
      )
        <$> M.toList bindings'

recursiveResolve ::
  Store ann ->
  StoreExpression ann ->
  Either StoreError [StoreExpression ann]
recursiveResolve store' storeExpr = do
  (ResolvedDeps deps) <- resolveDeps store' (storeBindings storeExpr)
  typeDeps <- resolveTypeStoreExpressions store' (storeTypeBindings storeExpr)
  let storeExprs = (\(_, (_, se)) -> se) <$> M.toList deps
      storeTypeExprs = (\(_, (_, se)) -> se) <$> M.toList typeDeps
      allStoreExprs = storeExprs <> storeTypeExprs
  subExprs <- traverse (recursiveResolve store') allStoreExprs
  pure $ mconcat subExprs <> allStoreExprs
