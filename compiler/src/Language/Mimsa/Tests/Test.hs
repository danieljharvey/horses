{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Tests.Test
  ( createTest,
    runTests,
    getDirectDeps,
    getDirectDepsOfTest,
  )
where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Tests.PropertyTest
import Language.Mimsa.Tests.UnitTest
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Tests

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

runTests ::
  (MonadIO m, MonadError (Error Annotation) m) =>
  Project Annotation ->
  Test ->
  m (TestResult Annotation)
runTests _ (UTest ut) = pure (UTestResult ut)
runTests project (PTest pt) =
  PTestResult pt <$> runPropertyTest project pt

getDirectDepsOfTest :: Project ann -> Test -> Set ExprHash
getDirectDepsOfTest prj test =
  let exprHash = case test of
        (UTest ut) -> utExprHash ut
        (PTest pt) -> ptExprHash pt
   in maybe mempty getDirectDeps (lookupExprHash prj exprHash)

getDirectDeps :: StoreExpression ann -> Set ExprHash
getDirectDeps storeExpr =
  S.fromList $
    M.elems (storeBindings storeExpr)
      <> M.elems (storeTypeBindings storeExpr)
