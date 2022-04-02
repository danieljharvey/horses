module Language.Mimsa.Transform.SimplifyPatterns where

import Data.Maybe
import Language.Mimsa.ExprUtils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers.TyCon

{-

RULE 1: reduce patterns (currently only works with 1 or 2 arg constructors)

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
simplifyPatterns (MyPatternMatch ann (MyApp _ (MyConstructor _ tc) argA) patterns) =
  let filterPatternExprs (pat, patExpr) =
        (,) <$> filterPattern tc pat
          <*> pure (simplifyPatterns patExpr)
   in MyPatternMatch ann argA (catMaybes (filterPatternExprs <$> patterns))
-- constructor with two args
simplifyPatterns (MyPatternMatch ann (MyApp appAnn (MyApp _ (MyConstructor _ tc) argA) argB) patterns) =
  let filterPatternExprs (pat, patExpr) =
        (,) <$> filterPattern tc pat
          <*> pure (simplifyPatterns patExpr)
   in MyPatternMatch
        ann
        (MyPair appAnn argA argB)
        (catMaybes (filterPatternExprs <$> patterns))
simplifyPatterns other = mapExpr simplifyPatterns other

filterPattern :: TyCon -> Pattern var ann -> Maybe (Pattern var ann)
filterPattern tc (PConstructor _ tc2 [a]) | tc == tc2 = Just a
filterPattern tc (PConstructor pAnn tc2 [a, b]) | tc == tc2 = Just (PPair pAnn a b)
filterPattern _ _ = Nothing
