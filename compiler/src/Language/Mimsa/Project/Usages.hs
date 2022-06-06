module Language.Mimsa.Project.Usages where

import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Project.Helpers (getCurrentBindings)
import Language.Mimsa.Store.ResolvedDeps (resolveDeps)
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Data.Map (Map)
import Language.Mimsa.Types.Modules.ModuleName
import Data.Bifunctor

findUsages :: Project ann -> ExprHash -> Either StoreError (Set Usage)
findUsages (Project store' bindings' _ _) =
  findUsages_ store' (toModuleBindings $ getCurrentBindings bindings')

toModuleBindings :: Bindings -> Map (Maybe ModuleName, Name) ExprHash
toModuleBindings (Bindings b) = M.fromList . fmap (first (\a -> (Nothing,a))) . M.toList $ b

findUsages_ :: Store ann -> Map (Maybe ModuleName, Name) ExprHash -> ExprHash -> Either StoreError (Set Usage)
findUsages_ store' bindings exprHash = do
  (ResolvedDeps resolvedDeps) <- resolveDeps store' bindings 
  let directDeps = mconcat $ addUsageIfMatching exprHash <$> M.toList resolvedDeps
  inDirectDeps <-
    traverse
      ( \((_,name'), (hash, storeExpr)) -> do
          subDeps <- findUsages_ store' (storeBindings storeExpr) exprHash
          if S.null subDeps
            then pure mempty
            else pure $ S.singleton (Transient name' hash)
      )
      (M.toList resolvedDeps)
  pure $ directDeps <> mconcat inDirectDeps

addUsageIfMatching ::
  ExprHash ->
  ((Maybe ModuleName, Name), (ExprHash, StoreExpression ann)) ->
  Set Usage
addUsageIfMatching exprHash ((_,name), (hash, storeExpr')) =
  let matchingNames = getMatches exprHash (storeBindings storeExpr')
   in if S.null matchingNames
        then mempty
        else S.singleton (Direct name hash)

-- list of names in some Bindings that use our hash
getMatches :: (Ord k) => ExprHash -> Map k  ExprHash -> Set k 
getMatches exprHash =
  S.fromList . M.keys . M.filter (exprHash ==) 
