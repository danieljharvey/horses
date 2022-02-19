module Language.Mimsa.Transform.FindUses (findUses, memberInUses, numberOfUses, Uses) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Language.Mimsa.ExprUtils
import Language.Mimsa.Types.AST

newtype Uses var = Uses (Map var (Sum Int))

instance (Ord var) => Semigroup (Uses var) where
  (Uses a) <> (Uses b) = Uses (M.unionWith (<>) a b)

instance (Ord var) => Monoid (Uses var) where
  mempty = Uses mempty

findUses :: (Ord var) => Expr var ann -> Uses var
findUses = withMonoid f
  where
    f (MyVar _ a) = (False, Uses (M.singleton a 1))
    f _ = (True, mempty)

memberInUses :: (Ord var) => var -> Uses var -> Bool
memberInUses var (Uses as) = M.member var as

numberOfUses :: (Ord var) => var -> Uses var -> Int
numberOfUses var (Uses as) = maybe 0 getSum (M.lookup var as)
