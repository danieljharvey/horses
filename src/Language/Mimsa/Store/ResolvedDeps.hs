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
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

-- we spend so much time passing the whole store around to match hashes
-- lets create one way of resolving a pile of them and be done with it

resolveDeps :: Store a -> Bindings -> Either [Name] (ResolvedDeps a)
resolveDeps (Store items) (Bindings bindings') =
  case partitionEithers foundItems of
    ([], found) -> Right (ResolvedDeps (M.fromList found))
    (fails, _) -> Left fails
  where
    foundItems =
      ( \(name, hash) -> case M.lookup hash items of
          Just storeExpr' -> Right (name, (hash, storeExpr'))
          Nothing -> Left name
      )
        <$> M.toList bindings'

extractDataType :: StoreExpression a -> Maybe DataType
extractDataType se =
  let types :: S.Set DataType
      types = extractDataTypes (storeExpression se)
   in listToMaybe . S.toList $ types

resolveTypeDeps :: Store a -> TypeBindings -> Either [TyCon] ResolvedTypeDeps
resolveTypeDeps (Store items) (TypeBindings bindings') =
  case partitionEithers foundItems of
    ([], found) -> Right (ResolvedTypeDeps (M.fromList found))
    (fails, _) -> Left fails
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

recursiveResolve :: Store a -> StoreExpression a -> Either [Name] [StoreExpression a]
recursiveResolve store' storeExpr = do
  (ResolvedDeps deps) <- resolveDeps store' (storeBindings storeExpr)
  let storeExprs = (\(name, (_, se)) -> (name, se)) <$> M.toList deps
  subExprs <- traverse (recursiveResolve store') (snd <$> storeExprs)
  pure $ mconcat subExprs <> (snd <$> storeExprs)
