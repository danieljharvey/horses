module Language.Mimsa.Transform.EtaReduce (etaReduce) where

import Language.Mimsa.ExprUtils
import Language.Mimsa.Transform.Shared
import Language.Mimsa.Types.AST

etaReduce :: (Eq ann, Eq var) => Expr var ann -> Expr var ann
etaReduce = repeatUntilEq etaReduceInternal

-- | turn `\a -> id a` into `id`
etaReduceInternal :: (Eq var) => Expr var ann -> Expr var ann
etaReduceInternal
  ( MyLambda
      _
      (Identifier _ varA)
      (MyApp _ fn (MyVar _ Nothing varB))
    )
    | varA == varB = fn
etaReduceInternal other = mapExpr etaReduceInternal other
