module Language.Mimsa.Transform.FloatDown (floatDown) where

import Data.Bifunctor (second)
import Language.Mimsa.Transform.FindUses
import Language.Mimsa.Transform.Shared
import Language.Mimsa.Core

-- if a let is above a pattern, it pushes it down into each branch of the
-- pattern match
-- this is so that it can be removed by dead code elimination on branches that
-- don't use it
floatDownInternal :: Expr Name ann -> Expr Name ann
floatDownInternal original@(MyLet ann ident expr (MyPatternMatch pAnn matchExpr pats)) =
  if memberInUses (extractIdentVar ident) Nothing (findUses matchExpr) -- if let var is in the matchExpr, don't float up
    then original
    else
      let newPatterns = second (MyLet ann ident expr) <$> pats
       in floatDownInternal $ MyPatternMatch pAnn matchExpr newPatterns
floatDownInternal other = mapExpr floatDownInternal other

floatDown :: (Eq ann) => Expr Name ann -> Expr Name ann
floatDown = repeatUntilEq floatDownInternal
