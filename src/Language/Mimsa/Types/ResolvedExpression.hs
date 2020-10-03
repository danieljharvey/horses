module Language.Mimsa.Types.ResolvedExpression where

import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.StoreExpression
import Language.Mimsa.Types.Swaps

data ResolvedExpression a
  = ResolvedExpression
      { reMonoType :: MonoType,
        reStoreExpression :: StoreExpression a,
        reExpression :: Expr a Variable,
        reScope :: Scope a,
        reSwaps :: Swaps
      }
