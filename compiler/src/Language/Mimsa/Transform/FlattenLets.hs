module Language.Mimsa.Transform.FlattenLets (flattenLets) where

import Language.Mimsa.ExprUtils
import Language.Mimsa.Types.AST

-- | We don't want `let a = (let b = 1 in b + 1) in a + 1`
-- instead we want `let b = 1; let a = b + 1; a + 1
flattenLets :: Expr var ann -> Expr var ann
flattenLets (MyLet ann ident (MyLet ann' ident' expr' body') body) =
  flattenLets $
    MyLet
      ann'
      ident'
      (flattenLets expr')
      ( MyLet
          ann
          ident
          (flattenLets body')
          (flattenLets body)
      )
-- make simple LetPatterns into Let to enable
-- further simplications
flattenLets (MyLetPattern ann (PVar pAnn var) expr body) =
  MyLet ann (Identifier pAnn var) (flattenLets expr) (flattenLets body)
-- flatten single pattern matches into let patterns to enable
-- further simplifications
flattenLets (MyPatternMatch ann expr [(pat, patExpr)]) =
  MyLetPattern ann pat (flattenLets expr) (flattenLets patExpr)
-- if we don't use expression, bin it
flattenLets (MyLetPattern _ (PWildcard _) _ body) = flattenLets body
flattenLets other = mapExpr flattenLets other
