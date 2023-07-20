{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Smol.Core.Interpreter.FindUses (findUses, memberInUses, numberOfUses, Uses (..)) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid
import Smol.Core.ExprUtils
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.ResolvedDep

newtype Uses = Uses (Map (ResolvedDep Identifier) (Sum Int))
  deriving newtype (Eq, Ord, Show)

instance Semigroup Uses where
  (Uses a) <> (Uses b) = Uses (M.unionWith (<>) a b)

instance Monoid Uses where
  mempty = Uses mempty

findUses :: Expr ResolvedDep ann -> Uses
findUses = withMonoid f
  where
    f (ELet _ ident body expr) =
      let usesInBody = clearVarFromUses ident (findUses body)
          usesInExpr = findUses expr
       in (False, usesInBody <> usesInExpr)
    f _ = (True, mempty)

-- | remove recursive uses of a var from it's body
clearVarFromUses :: ResolvedDep Identifier -> Uses -> Uses
clearVarFromUses var (Uses uses) =
  Uses (M.insert var (Sum 0) uses)

-- var in use and used over 0 times
memberInUses :: ResolvedDep Identifier -> Uses -> Bool
memberInUses var (Uses as) =
  maybe
    False
    (\(Sum a) -> a > 0)
    (M.lookup var as)

numberOfUses :: ResolvedDep Identifier -> Uses -> Int
numberOfUses var (Uses as) = maybe 0 getSum (M.lookup var as)
