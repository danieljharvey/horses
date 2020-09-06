module Language.Mimsa.Types.ResolvedDeps where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.StoreExpression

newtype ResolvedDeps
  = ResolvedDeps
      { getResolvedDeps ::
          Map Name
            (ExprHash, StoreExpression)
      }

hasNoDeps :: ResolvedDeps -> Bool
hasNoDeps (ResolvedDeps m) = M.size m == 0
