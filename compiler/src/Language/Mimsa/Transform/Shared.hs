module Language.Mimsa.Transform.Shared (extractIdentVar, findUses, repeatUntilEq) where

import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.ExprUtils
import Language.Mimsa.Types.AST

findUses :: (Ord var) => Expr var ann -> Set var
findUses = withMonoid f
  where
    f (MyVar _ a) = (False, S.singleton a)
    f _ = (True, mempty)

repeatUntilEq :: (Eq a) => (a -> a) -> a -> a
repeatUntilEq f a =
  let new = f a
   in if new == a then a else repeatUntilEq f new

extractIdentVar :: Identifier var ann -> var
extractIdentVar (Identifier _ name) = name
extractIdentVar (AnnotatedIdentifier _ name) = name
