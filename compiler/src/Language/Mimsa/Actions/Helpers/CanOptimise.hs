module Language.Mimsa.Actions.Helpers.CanOptimise where

import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Store

-- | can this expression be optimised?
canOptimise ::
  StoreExpression Annotation ->
  Actions.ActionM Bool
canOptimise se = do
  optimisedSe <- Actions.optimiseStoreExpression se
  pure (optimisedSe /= se)
