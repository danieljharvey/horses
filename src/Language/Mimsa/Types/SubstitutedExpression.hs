module Language.Mimsa.Types.SubstitutedExpression where

import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Swaps

data SubstitutedExpression
  = SubstitutedExpression
      { seSwaps :: Swaps,
        seExpr :: Expr Variable,
        seScope :: Scope
      }
  deriving (Eq, Ord, Show)
