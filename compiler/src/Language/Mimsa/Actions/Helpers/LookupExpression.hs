module Language.Mimsa.Actions.Helpers.LookupExpression
  ( lookupExpressionInStore,
    lookupExpression,
  )
where

import Control.Monad.Except
import Data.Functor
import qualified Data.Map as M
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

lookupExpression ::
  ExprHash ->
  Actions.ActionM (StoreExpression Annotation)
lookupExpression exprHash = do
  project <- Actions.getProject
  se <- lookupExpressionInStore (prjStore project) exprHash
  pure (se $> mempty)

-- | given a store, try and find something in it
lookupExpressionInStore ::
  Store ann ->
  ExprHash ->
  Actions.ActionM (StoreExpression ann)
lookupExpressionInStore store exprHash =
  case M.lookup exprHash (getStore store) of
    Just se -> pure se
    _ -> throwError (StoreErr (CouldNotFindStoreExpression exprHash))
