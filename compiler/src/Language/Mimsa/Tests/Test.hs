{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Tests.Test
  ( createTest,
    getTestsForExprHash,
    runTests,
    filterUnitTest,
    filterPropertyTest,
    createNewTests,
    getDirectDeps,
    getDirectDepsOfTest,
  )
where

import Control.Monad.Except
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (All (..), getAll)
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Store.UpdateDeps
import Language.Mimsa.Tests.PropertyTest
import Language.Mimsa.Tests.Types
import Language.Mimsa.Tests.UnitTest
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
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

-- | Get tests related to an expression
getTestsForExprHash ::
  Project ann ->
  ExprHash ->
  Map ExprHash Test
getTestsForExprHash prj exprHash =
  M.filter includeUnitTest (prjTests prj)
  where
    (currentHashes, oldHashes) =
      splitProjectHashesByVersion prj
    testDeps =
      getDirectDepsOfTest prj
    includeUnitTest ut =
      S.member exprHash (testDeps ut)
        && getAll (foldMap (All . depIsValid) (S.toList (testDeps ut)))
    depIsValid hash =
      hash == exprHash
        || S.member hash currentHashes
        || not (S.member hash oldHashes)

-- | get all hashes in project, split by current and old
splitProjectHashesByVersion ::
  Project ann ->
  (Set ExprHash, Set ExprHash)
splitProjectHashesByVersion prj =
  valBindings <> typeBindings
  where
    getItems as =
      mconcat $
        (\items -> (S.singleton (NE.head items), S.fromList (NE.tail items)))
          <$> as
    valBindings =
      let bindings = getVersionedMap $ prjBindings prj
       in getItems (M.elems bindings)
    typeBindings =
      let bindings = getVersionedMap $ prjTypeBindings prj
       in getItems (M.elems bindings)

filterUnitTest :: Test -> Maybe UnitTest
filterUnitTest = \case
  UTest ut -> Just ut
  PTest _ -> Nothing

filterPropertyTest :: Test -> Maybe PropertyTest
filterPropertyTest = \case
  PTest pt -> Just pt
  UTest _ -> Nothing

-- | When a new version of an expression is bound
-- create new versions of the tests
createNewTests ::
  Project Annotation ->
  ExprHash ->
  ExprHash ->
  Either (Error Annotation) (Project Annotation, [StoreExpression Annotation])
createNewTests project oldHash newHash = do
  -- get all tests and filter out UnitTests only
  let tests =
        getTestsForExprHash project oldHash
  newTests <-
    traverse
      (updateTest project oldHash newHash)
      (M.elems tests)
  let newProject =
        project
          <> mconcat
            ( (\(se, ut) -> fromTest ut se)
                <$> newTests
            )
  pure (newProject, fst <$> newTests)

getTestExprHash :: Test -> ExprHash
getTestExprHash (PTest pt) = ptExprHash pt
getTestExprHash (UTest ut) = utExprHash ut

getTestName :: Test -> TestName
getTestName (PTest pt) = ptName pt
getTestName (UTest ut) = utName ut

updateTest ::
  Project Annotation ->
  ExprHash ->
  ExprHash ->
  Test ->
  Either (Error Annotation) (StoreExpression Annotation, Test)
updateTest project oldHash newHash test = do
  case lookupExprHash project (getTestExprHash test) of
    Nothing ->
      throwError
        ( StoreErr $ CouldNotFindStoreExpression (getTestExprHash test)
        )
    Just testStoreExpr -> do
      let newBindings =
            updateExprHash testStoreExpr oldHash newHash
      newTestStoreExpr <-
        updateStoreExpressionBindings project newBindings testStoreExpr
      (,) newTestStoreExpr
        <$> createTest project newTestStoreExpr (getTestName test)

runTests ::
  (MonadIO m, MonadError (Error Annotation) m) =>
  Project Annotation ->
  Test ->
  m (TestResult Variable Annotation)
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
    M.elems (getBindings $ storeBindings storeExpr)
      <> M.elems (getTypeBindings $ storeTypeBindings storeExpr)
