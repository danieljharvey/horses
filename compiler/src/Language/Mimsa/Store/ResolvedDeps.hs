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
import Language.Mimsa.Types.Modules.ModuleName
import Language.Mimsa.Types.Store

-- given a list of bindings and the store, grab them all
resolveDeps ::
  Store ann ->
  Map (Maybe ModuleName, Name) ExprHash ->
  Either StoreError (ResolvedDeps ann)
resolveDeps (Store items) bindings' =
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

-- given a list of bindings and the store, grab them all
resolveInfixDeps ::
  Store ann ->
  Map InfixOp ExprHash ->
  Either StoreError (Map InfixOp (ExprHash, StoreExpression ann))
resolveInfixDeps (Store items) infixes =
  case partitionEithers foundItems of
    ([], found) -> Right (M.fromList found)
    (fails, _) -> Left $ CouldNotFindExprHashForInfixes fails
  where
    foundItems =
      ( \(infixOp, hash) -> case M.lookup hash items of
          Just storeExpr' ->
            Right (infixOp, (hash, storeExpr'))
          Nothing -> Left infixOp
      )
        <$> M.toList infixes

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

-- given a StoreExpression, get all the StoreExpressions it requires,
-- recursively
recursiveResolve ::
  Store ann ->
  StoreExpression ann ->
  Either StoreError [StoreExpression ann]
recursiveResolve store' storeExpr = do
  (ResolvedDeps deps) <- resolveDeps store' (storeBindings storeExpr)
  infixDeps <- resolveInfixDeps store' (storeInfixes storeExpr)
  typeDeps <- resolveTypeStoreExpressions store' (storeTypeBindings storeExpr)
  let storeExprs = snd <$> M.elems deps
      storeTypeExprs = snd <$> M.elems typeDeps
      storeInfixExprs = snd <$> M.elems infixDeps
      allStoreExprs = storeExprs <> storeTypeExprs <> storeInfixExprs
  subExprs <- traverse (recursiveResolve store') allStoreExprs
  pure $ mconcat subExprs <> allStoreExprs
