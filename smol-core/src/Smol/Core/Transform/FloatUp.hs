module Smol.Core.Transform.FloatUp (floatUp) where

import Smol.Core.ExprUtils
import Smol.Core.FindUses
import Smol.Core.Types

-- we try three times, which is somewhat arbitrary
floatUp :: (Ord (dep Identifier)) => Expr dep ann -> Expr dep ann
floatUp = floatUpInternal . floatUpInternal . floatUpInternal

floatUpInternal :: (Ord (dep Identifier)) => Expr dep ann -> Expr dep ann
floatUpInternal original@(ELambda ann ident (ELet ann' ident' expr body)) =
  let vars = findUses expr
   in if memberInUses ident vars -- if lambda var is in the expr, don't float up
        then original
        else ELet ann' ident' expr (ELambda ann ident body)
floatUpInternal other = mapExpr floatUpInternal other
