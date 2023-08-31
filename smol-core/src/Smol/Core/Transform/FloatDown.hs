module Smol.Core.Transform.FloatDown (floatDown) where

import Data.Bifunctor (second)
import Smol.Core
import Smol.Core.Helpers
import Smol.Core.Interpreter.FindUses

floatDown ::
  ( Ord (dep Identifier)
  ) =>
  Expr dep ann ->
  Expr dep ann
floatDown = nTimes 5 floatDownInternal

-- if a let is above a pattern, it pushes it down into each branch of the
-- pattern match
-- this is so that it can be removed by dead code elimination on branches that
-- don't use it
floatDownInternal :: (Ord (dep Identifier)) => Expr dep ann -> Expr dep ann
floatDownInternal
  original@( ELet
               ann
               ident
               expr
               (EPatternMatch pAnn matchExpr pats)
             ) =
    if memberInUses ident (findUses matchExpr) -- if let var is in the matchExpr, don't float down
      then original
      else
        let newPatterns = second (ELet ann ident expr) <$> pats
         in EPatternMatch pAnn matchExpr newPatterns
floatDownInternal
  original@( ELet
               ann
               ident
               expr
               (EIf ifAnn predExpr thenExpr elseExpr)
             ) =
    if memberInUses ident (findUses predExpr) -- if let var is in the matchExpr, don't float down
      then original
      else
        let newThenExpr = floatDownInternal (ELet ann ident expr thenExpr)
            newElseExpr = floatDownInternal (ELet ann ident expr elseExpr)
         in EIf ifAnn predExpr newThenExpr newElseExpr
floatDownInternal other = mapExpr floatDownInternal other
