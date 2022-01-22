module Language.Mimsa.Transform.FindUnused (findUnused, removeUnused) where

import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.ExprUtils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Typechecker

removeUnused :: Set var -> Expr var ann -> Expr var ann
removeUnused _remove expr = expr

findUnused :: (Ord var, Ord ann) => Expr var ann -> Set (var, ann)
findUnused expr =
  let uses = findUses expr
   in S.filter (\(var, _) -> not $ S.member var uses) (findVariables expr)

-- | find all variables introduced into expression
-- | we don't need to worry about shadowing because we'll have made everything
-- unique that needs to be in a previous step (otherwise typechecking would
-- choke)
findVariables :: (Ord var, Ord ann) => Expr var ann -> Set (var, ann)
findVariables = withMonoid f
  where
    f (MyLet _ (Identifier ann a) _ _) =
      (True, S.singleton (a, ann))
    f (MyLet _ (AnnotatedIdentifier mt a) _ _) =
      (True, S.singleton (a, getAnnotationForType mt))
    f (MyPatternMatch _ _ patterns) =
      (True, mconcat (findVariableInPattern . fst <$> patterns))
    f _other = (True, mempty)

-- | Find all variables in pattern match
findVariableInPattern :: (Ord var, Ord ann) => Pattern var ann -> Set (var, ann)
findVariableInPattern (PVar ann a) =
  S.singleton (a, ann)
findVariableInPattern (PPair _ a b) =
  findVariableInPattern a <> findVariableInPattern b
findVariableInPattern (PConstructor _ _ vars) =
  mconcat (findVariableInPattern <$> vars)
findVariableInPattern (PRecord _ as) =
  mconcat (findVariableInPattern <$> M.elems as)
findVariableInPattern (PArray _ as spread) =
  let spreadVars = case spread of
        SpreadValue ann a -> S.singleton (a, ann)
        _ -> mempty
   in mconcat (findVariableInPattern <$> as) <> spreadVars
findVariableInPattern (PString _ sHead sTail) =
  let findStringPartVar var = case var of
        StrValue ann a -> S.singleton (a, ann)
        _ -> mempty
   in findStringPartVar sHead <> findStringPartVar sTail
findVariableInPattern PWildcard {} = mempty
findVariableInPattern PLit {} = mempty

findUses :: (Ord var) => Expr var ann -> Set var
findUses = withMonoid f
  where
    f (MyVar _ a) = (False, S.singleton a)
    f _ = (True, mempty)
