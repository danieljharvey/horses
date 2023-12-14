module Smol.Core.Transform.FlattenLets (flattenLets) where

import Smol.Core.ExprUtils
import Smol.Core.Types

-- | We don't want `let a = (let b = 1 in b + 1) in a + 1`
-- instead we want `let b = 1; let a = b + 1; a + 1
flattenLets :: (Eq (dep Identifier)) => Expr dep ann -> Expr dep ann
flattenLets (ELet ann ident (ELet ann' ident' expr' body') body) =
  flattenLets $
    ELet
      ann'
      ident'
      (flattenLets expr')
      ( ELet
          ann
          ident
          (flattenLets body')
          (flattenLets body)
      )
-- also, turn `let a = b in a` into `b`
flattenLets (ELet _ ident body (EVar _ ident')) | ident == ident' = body
flattenLets other = mapExpr flattenLets other
