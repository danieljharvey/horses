module Language.Mimsa.Transform.SimplifyPatterns where

import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Language.Mimsa.ExprUtils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers.TyCon

{-

RULE 1: reduce patterns (currently only works with 1 or 2 arg constructors)

TODO: we can do loads now because of tuples

match (Just (("dog", "log"))) with
    (Just (b, c)) ->
      b ++ c
  | _ ->
      ""

should becomes

match ("dog", "log") with
    (b, c) ->
      b ++ c

1) if the match arm is known, reduce any outer constructors
2) then remove them from patterns below
3) and remove ones that are no longer relevant

-}

simplifyPatterns :: Expr var ann -> Expr var ann
-- constructor with single arg
simplifyPatterns orig@(MyPatternMatch ann (MyApp _ (MyConstructor _ _ tc) argA) patterns) =
  case filterPatterns tc patterns of
    Just newPatterns ->
      MyPatternMatch ann argA newPatterns
    Nothing -> orig
-- constructor with two args
simplifyPatterns orig@(MyPatternMatch ann (MyApp appAnn (MyApp _ (MyConstructor _ _ tc) argA) argB) patterns) =
  case filterPatterns tc patterns of
    Just newPatterns ->
      MyPatternMatch ann (MyTuple appAnn argA (NE.singleton argB)) newPatterns
    Nothing -> orig
-- otherwise look through expr looking for more
simplifyPatterns other = mapExpr simplifyPatterns other

filterPatterns :: TyCon -> [(Pattern var ann, Expr var ann)] -> Maybe [(Pattern var ann, Expr var ann)]
filterPatterns tc pats =
  let filterPatternExprs (pat, patExpr) =
        (,)
          <$> filterPattern tc pat
          <*> pure (simplifyPatterns patExpr)
      filtered = mapMaybe filterPatternExprs pats
   in if null filtered
        then Nothing
        else Just (removeDuplicateWildcards filtered)

-- we're creating more general patterns so may need to remove our previous
-- catches
removeDuplicateWildcards ::
  [(Pattern var ann, Expr var ann)] ->
  [(Pattern var ann, Expr var ann)]
removeDuplicateWildcards =
  snd
    . foldl'
      ( \(found, pats) thisPat -> case thisPat of
          (PWildcard _, _) ->
            if found
              then (found, pats)
              else (True, pats <> [thisPat])
          (PVar _ _, _) ->
            if found
              then (found, pats)
              else (True, pats <> [thisPat])
          _ -> (found, pats <> [thisPat])
      )
      (False, mempty)

filterPattern :: TyCon -> Pattern var ann -> Maybe (Pattern var ann)
filterPattern tc (PConstructor _ _ tc2 [a]) | tc == tc2 = Just a -- TODO: check this works with namespace
filterPattern tc (PConstructor pAnn _ tc2 [a, b]) | tc == tc2 = Just (PTuple pAnn a (NE.singleton b))
filterPattern _ (PWildcard ann) = Just (PWildcard ann)
filterPattern _ (PVar ann var) = Just (PVar ann var)
filterPattern _ _ = Nothing
