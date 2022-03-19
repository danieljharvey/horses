module Language.Mimsa.Project.Usages where

import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Project.Persistence (getCurrentBindings)
import Language.Mimsa.Store.ResolvedDeps (resolveDeps)
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

findUsages :: Project ann -> ExprHash -> Either StoreError (Set Usage)
findUsages (Project store' bindings' _ _) =
  findUsages_ store' (getCurrentBindings bindings')

findUsages_ :: Store ann -> Bindings -> ExprHash -> Either StoreError (Set Usage)
findUsages_ store' bindings' exprHash = do
  (ResolvedDeps resolvedDeps) <- resolveDeps store' bindings'
  let directDeps = mconcat $ addUsageIfMatching exprHash <$> M.toList resolvedDeps
  inDirectDeps <-
    traverse
      ( \(name', (hash, storeExpr)) -> do
          subDeps <- findUsages_ store' (storeBindings storeExpr) exprHash
          if S.null subDeps
            then pure mempty
            else pure $ S.singleton (Transient name' hash)
      )
      (M.toList resolvedDeps)
  pure $ directDeps <> mconcat inDirectDeps

addUsageIfMatching ::
  ExprHash ->
  (Name, (ExprHash, StoreExpression ann)) ->
  Set Usage
addUsageIfMatching exprHash (name, (hash, storeExpr')) =
  let matchingNames = getMatches exprHash (storeBindings storeExpr')
   in if S.null matchingNames
        then mempty
        else S.singleton (Direct name hash)

-- list of names in some Bindings that use our hash
getMatches :: ExprHash -> Bindings -> Set Name
getMatches exprHash (Bindings bindings') =
  S.fromList . M.keys . M.filter (exprHash ==) $ bindings'
