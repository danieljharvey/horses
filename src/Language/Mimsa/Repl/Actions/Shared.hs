{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions.Shared
  ( saveExpression,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Language.Mimsa.Repl.Types
import Language.Mimsa.Store
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Store

saveExpression ::
  StoreExpression Annotation ->
  ReplM Annotation ExprHash
saveExpression storeExpr = do
  mimsaConfig <- ask
  lift $ withExceptT StoreErr $ saveExpr mimsaConfig storeExpr
