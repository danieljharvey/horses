module Language.Mimsa.Tests.Test
  ( createTest,
    getTestsForExprHash,
  )
where

import Data.Map (Map)
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
  Project Annotation ->
  StoreExpression Annotation ->
  TestName ->
  Either (Error Annotation) Test
createTest project storeExpr testName =
  UTest
    <$> createUnitTest project storeExpr testName

getTestsForExprHash :: Project ann -> ExprHash -> Map ExprHash Test
getTestsForExprHash prj exprHash =
  UTest <$> getUnitTestsForExprHash prj exprHash
