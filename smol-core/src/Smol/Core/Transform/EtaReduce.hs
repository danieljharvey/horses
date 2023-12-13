module Smol.Core.Transform.EtaReduce (etaReduce) where

import Smol.Core.Types
import Smol.Core.ExprUtils

etaReduce :: (Eq (dep Identifier)) => Expr dep ann -> Expr dep ann
etaReduce = etaReduceInternal

-- | turn `\a -> id a` into `id`
etaReduceInternal ::
  (Eq (dep Identifier)) =>
  Expr dep ann ->
  Expr dep ann
etaReduceInternal (ELambda _ varA (EApp _ fn (EVar _ varA')))
  | varA == varA' =
      etaReduceInternal fn
etaReduceInternal
  ( ELambda
      _
      varA
      (ELambda _ varB (EApp _ (EApp _ fn (EVar _ varA')) (EVar _ varB')))
    )
    | varA == varA' && varB == varB' =
        etaReduceInternal fn
etaReduceInternal
  ( ELambda
      _
      varA
      ( ELambda
          _
          varB
          ( ELambda
              _
              varC
              ( EApp
                  _
                  (EApp _ (EApp _ fn (EVar _ varA')) (EVar _ varB'))
                  (EVar _ varC')
                )
            )
        )
    )
    | varA == varA' && varB == varB' && varC == varC' =
        etaReduceInternal fn
etaReduceInternal other = mapExpr etaReduceInternal other
