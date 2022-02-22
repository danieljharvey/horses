module Language.Mimsa.Transform.FloatUp (floatUp) where

import Language.Mimsa.ExprUtils
import Language.Mimsa.Transform.FindUses
import Language.Mimsa.Transform.Shared
import Language.Mimsa.Types.AST

-- if a let is above a pattern, it pushes it down into each branch of the
-- pattern match
-- this is so that it can be removed by dead code elimination on branches that
-- don't use it
floatUpInternal :: (Ord var) => Expr var ann -> Expr var ann
floatUpInternal original@(MyLambda ann ident (MyLet ann' ident' expr body)) =
  let lambdaVar = extractIdentVar ident
      vars = findUses expr
   in if memberInUses lambdaVar vars -- if lambda var is in the expr, don't float up
        then original
        else MyLet ann' ident' expr (MyLambda ann ident body)
floatUpInternal other = mapExpr floatUpInternal other

floatUp :: (Eq ann, Ord var) => Expr var ann -> Expr var ann
floatUp = repeatUntilEq floatUpInternal
