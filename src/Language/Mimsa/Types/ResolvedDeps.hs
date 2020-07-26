module Language.Mimsa.Types.ResolvedDeps where

import Data.Map (Map)
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Store

newtype ResolvedDeps = ResolvedDeps {getResolvedDeps :: Map Name StoreExpression}
