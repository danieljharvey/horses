module Smol.Core.Transform.ConstantFold (constantFold) where

import Smol.Core.ExprUtils
import Smol.Core.Transform.Helpers (repeatUntilEq)
import Smol.Core.Types

constantFold ::
  ( Eq ann,
    Eq (dep Identifier),
    Eq (dep TypeName),
    Eq (dep Constructor)
  ) =>
  Expr dep ann ->
  Expr dep ann
constantFold = foldLots

foldLots :: (Eq ann, Eq (dep TypeName), Eq (dep Constructor), Eq (dep Identifier)) => Expr dep ann -> Expr dep ann
foldLots = repeatUntilEq constantFoldInternal

constantFoldInternal ::
  ( Eq ann,
    Eq (dep TypeName),
    Eq (dep Constructor),
    Eq (dep Identifier)
  ) =>
  Expr dep ann ->
  Expr dep ann
-- add ints
constantFoldInternal (EInfix ann OpAdd (EPrim _ (PInt i)) (EPrim _ (PInt j))) =
  EPrim ann (PInt $ i + j)
-- turn `1 + a` into `a + 1`
constantFoldInternal (EInfix ann op prim@(EPrim {}) var@(EVar {})) =
  EInfix ann op var prim
-- turn `infix + a` into `a + infix`
constantFoldInternal (EInfix ann op inf@(EInfix {}) var@(EVar {})) =
  foldLots (EInfix ann op var (foldLots inf))
-- turn `(a + 1) + 1` into `a + (1 + 1)` (enables the rest of these to work)
constantFoldInternal (EInfix ann OpAdd (EInfix ann' op' var@(EVar {}) otherA) otherB) =
  EInfix ann OpAdd var (foldLots $ EInfix ann' op' otherA otherB)
constantFoldInternal other = mapExpr foldLots other
