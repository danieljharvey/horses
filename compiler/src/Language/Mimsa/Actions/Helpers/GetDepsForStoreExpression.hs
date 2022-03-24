{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Actions.Helpers.GetDepsForStoreExpression
  ( getDepsForStoreExpression,
  )
where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

----------

-- recursively get all the StoreExpressions required
getDepsForStoreExpression ::
  StoreExpression Annotation ->
  Actions.ActionM (Map ExprHash (StoreExpression Annotation, Text))
getDepsForStoreExpression storeExpr = do
  project <- Actions.getProject
  depsList <-
    liftEither $
      first
        StoreErr
        (recursiveResolve (prjStore project) storeExpr)
  pure $
    M.singleton (getStoreExpressionHash storeExpr) (storeExpr, prettyPrint storeExpr)
      <> M.fromList
        ( ( \se ->
              (getStoreExpressionHash se, (se, prettyPrint se))
          )
            <$> depsList
        )
