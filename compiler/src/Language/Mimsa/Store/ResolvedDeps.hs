module Language.Mimsa.Store.ResolvedDeps
  ( resolveDeps,
    resolveTypeDeps,
    recursiveResolve,
  )
where

import Data.Either (partitionEithers)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Language.Mimsa.Store.ExtractTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
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
          Just storeExpr' -> Right (name, (hash, storeExpr'))
          Nothing -> Left name
      )
        <$> M.toList bindings'

extractDataType ::
  (Ord ann) =>
  StoreExpression ann ->
  Maybe (DataType ann)
extractDataType se =
  let types = extractDataTypes (storeExpression se)
   in listToMaybe . S.toList $ types

resolveTypeDeps ::
  (Ord ann) =>
  Store ann ->
  TypeBindings ->
  Either StoreError (ResolvedTypeDeps ann)
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

recursiveResolve ::
  Store ann ->
  StoreExpression ann ->
  Either StoreError [StoreExpression ann]
recursiveResolve store' storeExpr = do
  (ResolvedDeps deps) <- resolveDeps store' (storeBindings storeExpr)
  let storeExprs = (\(name, (_, se)) -> (name, se)) <$> M.toList deps
  subExprs <- traverse (recursiveResolve store') (snd <$> storeExprs)
  pure $ mconcat subExprs <> (snd <$> storeExprs)
