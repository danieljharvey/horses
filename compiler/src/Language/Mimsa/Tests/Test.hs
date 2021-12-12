{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Tests.Test
  ( createTest,
    getTestsForExprHash,
  )
where

import Control.Monad.Except
import Data.Map (Map)
import Language.Mimsa.Tests.PropertyTest
import Language.Mimsa.Tests.Types
import Language.Mimsa.Tests.UnitTest
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

{- we have multiple types of test, this module operates on all kinds
-}

-- | a unit test must have type Boolean
-- | a property test must have type \\something -> Boolean
-- | this function tries both
createTest ::
  (MonadError (Error Annotation) m) =>
  Project Annotation ->
  StoreExpression Annotation ->
  TestName ->
  m Test
createTest project storeExpr testName = do
  case createUnitTest project storeExpr testName of
    Right ut -> pure (UTest ut)
    Left _e -> case createPropertyTest project storeExpr testName of
      Right pt -> pure (PTest pt)
      Left e -> throwError e

getTestsForExprHash :: Project ann -> ExprHash -> Map ExprHash Test
getTestsForExprHash prj exprHash =
  UTest <$> getUnitTestsForExprHash prj exprHash
