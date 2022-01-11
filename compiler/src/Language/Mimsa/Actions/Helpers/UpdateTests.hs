module Language.Mimsa.Actions.Helpers.UpdateTests (updateTests) where

import Control.Monad.Except (liftEither)
import Data.Foldable (traverse_)
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Tests.Test
import Language.Mimsa.Types.Store

-- given an old and a new ExprHash, copy any tests from the old to the
-- new
updateTests :: ExprHash -> ExprHash -> Actions.ActionM Int
updateTests oldExprHash newExprHash = do
  project <- Actions.getProject
  (projectWithNewTests, newExprs) <-
    liftEither $
      createNewTests project oldExprHash newExprHash
  traverse_ Actions.appendStoreExpression newExprs
  Actions.setProject projectWithNewTests
  pure (length newExprs)
