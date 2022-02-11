module Language.Mimsa.Transform.FindUnused (findUnused, removeBindings, removeUnused) where

import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.ExprUtils
import Language.Mimsa.Transform.Shared
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

removeUnused :: (Ord ann) => Expr Variable ann -> Expr Variable ann
removeUnused = repeatUntilEq removeUnusedInternal

removeUnusedInternal :: (Ord ann) => Expr Variable ann -> Expr Variable ann
removeUnusedInternal expr =
  let unused = findUnused expr
   in removeBindings (S.map fst unused) expr

removeBindings :: Set Variable -> Expr Variable ann -> Expr Variable ann
removeBindings remove = f
  where
    f wholeExpr@(MyLet _ ident _ letBody) =
      let a = case ident of
            Identifier _ var -> var
            AnnotatedIdentifier _ var -> var
       in if S.member a remove
            then letBody
            else wholeExpr
    f (MyPatternMatch ann expr patterns) =
      let tidyExpr = removeBindings remove expr
          tidyPattern (pat, patExpr) =
            ( removeBindingsInPattern remove pat,
              removeBindings remove patExpr
            )
       in MyPatternMatch ann tidyExpr (tidyPattern <$> patterns)
    f (MyLetPattern ann pat expr body) =
      MyLetPattern
        ann
        (removeBindingsInPattern remove pat)
        (removeBindings remove expr)
        (removeBindings remove body)
    f other = mapExpr f other

removeBindingsInPattern ::
  Set Variable ->
  Pattern Variable ann ->
  Pattern Variable ann
removeBindingsInPattern remove = f
  where
    f wholePat@(PVar ann a) =
      if S.member a remove
        then PWildcard ann
        else wholePat
    f other = mapPattern f other

findUnused :: (Ord ann) => Expr Variable ann -> Set (Variable, ann)
findUnused expr =
  let uses = findUses expr
   in S.filter (\(var, _) -> not $ S.member var uses) (findVariables expr)

-- | find all variables introduced into expression
-- | we don't need to worry about shadowing because we'll have made everything
-- unique that needs to be in a previous step (otherwise typechecking would
-- choke)
findVariables :: (Ord ann) => Expr Variable ann -> Set (Variable, ann)
findVariables = withMonoid f
  where
    f (MyLet _ (Identifier ann a) _ _) =
      (True, S.singleton (a, ann))
    f (MyLet _ (AnnotatedIdentifier mt a) _ _) =
      (True, S.singleton (a, getAnnotationForType mt))
    f (MyPatternMatch _ _ patterns) =
      (True, mconcat (findVariableInPattern . fst <$> patterns))
    f (MyLetPattern _ pat _ _) =
      (True, findVariableInPattern pat)
    f _other = (True, mempty)

-- | Find all variables in pattern match
findVariableInPattern :: (Ord ann) => Pattern Variable ann -> Set (Variable, ann)
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
