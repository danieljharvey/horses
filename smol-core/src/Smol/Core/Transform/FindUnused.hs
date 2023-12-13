module Smol.Core.Transform.FindUnused (findUnused, removeBindings, removeUnused) where

import Data.Set (Set)
import qualified Data.Set as S
import Smol.Core.Types
import Smol.Core.FindUses
import qualified Data.List.NonEmpty as NE
import Smol.Core.ExprUtils

removeUnused ::
  (Ord ann, Ord (dep Identifier)) =>
  Expr dep ann ->
  Expr dep ann
removeUnused expr =
  let unused = findUnused expr
   in removeBindings (S.map fst unused) expr

removeBindings :: (Ord (dep Identifier)) =>
  Set (dep Identifier) -> Expr dep ann -> Expr dep ann
removeBindings remove = f
  where
    f (ELet ann ident letExpr letBody) =
       if S.member ident remove
            then letBody
            else ELet ann ident (f letExpr) (f letBody)
    f (EPatternMatch ann expr patterns) =
      let tidyPattern (pat, patExpr) =
            ( removeBindingsInPattern remove pat,
              f patExpr
            )
       in EPatternMatch ann (f expr) (tidyPattern <$> patterns)
    f other = mapExpr f other

removeBindingsInPattern ::
  (Ord (dep Identifier)) =>
  Set (dep Identifier) ->
  Pattern dep ann ->
  Pattern dep ann
removeBindingsInPattern remove = f
  where
    f wholePat@(PVar ann a) =
      if S.member a remove
        then PWildcard ann
        else wholePat
    f (PArray ann pParts pSpread) =
      let rmSpread = case pSpread of
            SpreadValue sprAnn' a ->
              if S.member a remove
                then SpreadWildcard sprAnn'
                else pSpread
            other -> other
       in PArray ann (f <$> pParts) rmSpread
    f other = mapPattern f other

findUnused :: (Ord ann, Ord (dep Identifier) ) =>
    Expr dep ann -> Set (dep Identifier, ann)
findUnused expr =
  let uses = findUses expr
   in S.filter (\(var, _) -> not $ memberInUses var uses) (findVariables expr)

-- | find all variables introduced into expression
-- | we don't need to worry about shadowing because we'll have made everything
-- unique that needs to be in a previous step (otherwise typechecking would
-- choke)
findVariables :: (Ord ann, Ord (dep Identifier)) =>
    Expr dep ann -> Set (dep Identifier, ann)
findVariables = withMonoid f
  where
    f (ELet ann ident  _ _) =
      (True, S.singleton (ident, ann))
    f (EPatternMatch _ _ patterns) =
      (True, mconcat (findVariableInPattern . fst <$> NE.toList patterns))
    f _other = (True, mempty)

-- | Find all variables in pattern match
findVariableInPattern :: (Ord ann, Ord (dep Identifier) ) =>
    Pattern dep ann -> Set (dep Identifier, ann)
findVariableInPattern (PVar ann a) =
  S.singleton (a, ann)
findVariableInPattern (PTuple _ a as) =
  findVariableInPattern a <> foldMap findVariableInPattern as
findVariableInPattern (PConstructor _ _ vars) =
  mconcat (findVariableInPattern <$> vars)
findVariableInPattern (PArray _ as spread) =
  let spreadVars = case spread of
        SpreadValue ann a -> S.singleton (a, ann)
        _ -> mempty
   in mconcat (findVariableInPattern <$> as) <> spreadVars
findVariableInPattern PWildcard {} = mempty
findVariableInPattern PLiteral {} = mempty

