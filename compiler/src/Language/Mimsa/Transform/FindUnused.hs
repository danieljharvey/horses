module Language.Mimsa.Transform.FindUnused where

import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.ExprUtils
import Language.Mimsa.Types.AST

findUnused :: (Ord var) => Expr var ann -> Set var
findUnused expr = S.difference (findVariables expr) (findUses expr)

-- | find all variables introduced into expression
-- | we don't need to worry about shadowing because we'll have made everything
-- unique that needs to be in a previous step (otherwise typechecking would
-- choke)
findVariables :: (Ord var) => Expr var ann -> Set var
findVariables = withMonoid f
  where
    f (MyLet _ (Identifier _ a) _ _) = (True, S.singleton a)
    f (MyLet _ (AnnotatedIdentifier _ a) _ _) = (True, S.singleton a)
    f _other = (True, mempty)

findUses :: (Ord var) => Expr var ann -> Set var
findUses = withMonoid f
  where
    f (MyVar _ a) = (False, S.singleton a)
    f _ = (True, mempty)
