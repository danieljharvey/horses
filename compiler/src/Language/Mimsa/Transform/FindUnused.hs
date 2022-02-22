module Language.Mimsa.Transform.FindUnused (findUnused, removeBindings, removeUnused) where

import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.ExprUtils
import Language.Mimsa.Transform.FindUses
import Language.Mimsa.Transform.Shared
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Typechecker

removeUnused :: (Ord ann, Ord var) => Expr var ann -> Expr var ann
removeUnused = repeatUntilEq removeUnusedInternal

removeUnusedInternal ::
  (Ord ann, Ord var) =>
  Expr var ann ->
  Expr var ann
removeUnusedInternal expr =
  let unused = findUnused expr
   in removeBindings (S.map fst unused) expr

removeBindings :: (Ord var) => Set var -> Expr var ann -> Expr var ann
removeBindings remove = f
  where
    f (MyLet ann ident letExpr letBody) =
      let a = case ident of
            Identifier _ var -> var
            AnnotatedIdentifier _ var -> var
       in if S.member a remove
            then letBody
            else MyLet ann ident (f letExpr) (f letBody)
    f (MyPatternMatch ann expr patterns) =
      let tidyPattern (pat, patExpr) =
            ( removeBindingsInPattern remove pat,
              f patExpr
            )
       in MyPatternMatch ann (f expr) (tidyPattern <$> patterns)
    f (MyLetPattern ann pat expr body) =
      MyLetPattern
        ann
        (removeBindingsInPattern remove pat)
        (f expr)
        (f body)
    f other = mapExpr f other

removeBindingsInPattern ::
  (Ord var) =>
  Set var ->
  Pattern var ann ->
  Pattern var ann
removeBindingsInPattern remove = f
  where
    f wholePat@(PVar ann a) =
      if S.member a remove
        then PWildcard ann
        else wholePat
    f (PString ann pHead pTail) =
      let removeFromStringPart sp = case sp of
            StrValue strAnn a ->
              if S.member a remove
                then StrWildcard strAnn
                else sp
            other -> other
       in PString
            ann
            (removeFromStringPart pHead)
            (removeFromStringPart pTail)
    f (PArray ann pParts pSpread) =
      let rmSpread = case pSpread of
            SpreadValue sprAnn' a ->
              if S.member a remove
                then SpreadWildcard sprAnn'
                else pSpread
            other -> other
       in PArray ann (f <$> pParts) rmSpread
    f other = mapPattern f other

findUnused :: (Ord ann, Ord var) => Expr var ann -> Set (var, ann)
findUnused expr =
  let uses = findUses expr
   in S.filter (\(var, _) -> not $ memberInUses var uses) (findVariables expr)

-- | find all variables introduced into expression
-- | we don't need to worry about shadowing because we'll have made everything
-- unique that needs to be in a previous step (otherwise typechecking would
-- choke)
findVariables :: (Ord ann, Ord var) => Expr var ann -> Set (var, ann)
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
findVariableInPattern :: (Ord ann, Ord var) => Pattern var ann -> Set (var, ann)
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
