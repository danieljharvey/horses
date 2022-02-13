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
flattenLets other = mapExpr flattenLets other
