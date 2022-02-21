{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Mimsa.Transform.FindUses (findUses, memberInUses, numberOfUses, Uses (..)) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Language.Mimsa.ExprUtils
import Language.Mimsa.Types.AST

newtype Uses var = Uses (Map var (Sum Int))
  deriving newtype (Eq, Ord, Show)

instance (Ord var) => Semigroup (Uses var) where
  (Uses a) <> (Uses b) = Uses (M.unionWith (<>) a b)

instance (Ord var) => Monoid (Uses var) where
  mempty = Uses mempty

findUses :: (Ord var) => Expr var ann -> Uses var
findUses = withMonoid f
  where
    f (MyLet _ ident body expr) =
      let var = nameFromIdent ident
          usesInBody = clearVarFromUses var (findUses body)
          usesInExpr = findUses expr
       in (False, usesInBody <> usesInExpr)
    f (MyVar _ a) = (False, Uses (M.singleton a 1))
    f _ = (True, mempty)

-- | remove recursive uses of a var from it's body
clearVarFromUses :: (Ord var) => var -> Uses var -> Uses var
clearVarFromUses var (Uses uses) =
  Uses (M.insert var (Sum 0) uses)

-- var in use and used over 0 times
memberInUses :: (Ord var) => var -> Uses var -> Bool
memberInUses var (Uses as) =
  maybe
    False
    (\(Sum a) -> a > 0)
    (M.lookup var as)

numberOfUses :: (Ord var) => var -> Uses var -> Int
numberOfUses var (Uses as) = maybe 0 getSum (M.lookup var as)
