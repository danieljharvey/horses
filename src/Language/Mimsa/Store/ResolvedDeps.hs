module Language.Mimsa.Store.ResolvedDeps (resolveDeps) where

import Data.Either (partitionEithers)
import qualified Data.Map as M
import Language.Mimsa.Types

-- we spend so much time passing the whole store around to match hashes
-- lets create one way of resolving a pile of them and be done with it

resolveDeps :: Store -> Bindings -> Either [Name] ResolvedDeps
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
        <$> (M.toList bindings')
