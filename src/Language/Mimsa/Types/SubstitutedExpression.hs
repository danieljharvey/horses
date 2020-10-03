module Language.Mimsa.Types.SubstitutedExpression where

import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Swaps

data SubstitutedExpression a
  = SubstitutedExpression
      { seSwaps :: Swaps,
        seExpr :: Expr a Variable,
        seScope :: Scope a
      }
  deriving (Eq, Ord, Show)
