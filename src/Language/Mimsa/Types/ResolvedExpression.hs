module Language.Mimsa.Types.ResolvedExpression where

import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Store.StoreExpression
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker.MonoType

data ResolvedExpression ann
  = ResolvedExpression
      { reMonoType :: MonoType,
        reStoreExpression :: StoreExpression ann,
        reExpression :: Expr Variable ann,
        reScope :: Scope ann,
        reSwaps :: Swaps
      }
