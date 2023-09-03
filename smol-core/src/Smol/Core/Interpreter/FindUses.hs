{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Smol.Core.Interpreter.FindUses (findUses, memberInUses, numberOfUses, Uses (..)) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid
import Smol.Core.ExprUtils
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier

newtype Uses dep = Uses (Map (dep Identifier) (Sum Int))

deriving newtype instance (Eq (dep Identifier)) => Eq (Uses dep)

deriving newtype instance (Ord (dep Identifier)) => Ord (Uses dep)

deriving newtype instance (Show (dep Identifier)) => Show (Uses dep)

instance (Ord (dep Identifier)) => Semigroup (Uses dep) where
  (Uses a) <> (Uses b) = Uses (M.unionWith (<>) a b)

instance (Ord (dep Identifier)) => Monoid (Uses dep) where
  mempty = Uses mempty

findUses :: (Ord (dep Identifier)) => Expr dep ann -> Uses dep
findUses = withMonoid f
  where
    f (ELet _ ident body expr) =
      let usesInBody = clearVarFromUses ident (findUses body)
          usesInExpr = findUses expr
       in (False, usesInBody <> usesInExpr)
    f (EVar _ ident) = (False, Uses $ M.singleton ident 1)
    f _ = (True, mempty)

-- | remove recursive uses of a var from it's body
clearVarFromUses ::
  (Ord (dep Identifier)) =>
  dep Identifier ->
  Uses dep ->
  Uses dep
clearVarFromUses var (Uses uses) =
  Uses (M.insert var (Sum 0) uses)

-- var in use and used over 0 times
memberInUses :: (Ord (dep Identifier)) => dep Identifier -> Uses dep -> Bool
memberInUses var (Uses as) =
  maybe
    False
    (\(Sum a) -> a > 0)
    (M.lookup var as)

numberOfUses :: (Ord (dep Identifier)) => dep Identifier -> Uses dep -> Int
numberOfUses var (Uses as) = maybe 0 getSum (M.lookup var as)
