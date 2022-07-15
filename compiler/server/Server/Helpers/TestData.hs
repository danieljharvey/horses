{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Helpers.TestData
  ( UnitTestData (..),
    PropertyTestData (..),
    TestData (..),
    makeTestData,
    mkUnitTestData,
    makeTestDataFromModule,
  )
where

import qualified Data.Aeson as JSON
import Data.Coerce
import Data.Either
import Data.Map (Map)
import qualified Data.Map as M
import Data.OpenApi hiding (get)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Project
import Language.Mimsa.Tests.Test
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Tests

data TestData = TestData
  { tdUnitTests :: [UnitTestData],
    tdPropertyTests :: [PropertyTestData]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

data UnitTestData = UnitTestData
  { utdTestName :: Text,
    utdTestSuccess :: Bool,
    utdBindings :: Map Name Text -- bin this off once we're doing modules
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

mkUnitTestData :: Project ann -> UnitTest -> UnitTestData
mkUnitTestData project unitTest = do
  let getDep = (`findBindingNameForExprHash` project)
  let depMap =
        mconcat
          ( getDep
              <$> S.toList
                (getDirectDepsOfTest project (UTest unitTest))
          )
  UnitTestData
    (coerce $ utName unitTest)
    (coerce $ utSuccess unitTest)
    (coerce <$> depMap)

data PropertyTestData = PropertyTestData
  { ptdTestName :: Text,
    ptdTestFailures :: [Text],
    ptdBindings :: Map Name Text -- bin this off once we're doing modules
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

mkPropertyTestData ::
  Project ann ->
  PropertyTest ->
  PropertyTestResult ann ->
  PropertyTestData
mkPropertyTestData project propertyTest result = do
  let getDep = (`findBindingNameForExprHash` project)
  let depMap =
        mconcat
          ( getDep
              <$> S.toList
                (getDirectDepsOfTest project (PTest propertyTest))
          )
  let failures = case result of
        PropertyTestSuccess -> mempty
        PropertyTestFailures es -> prettyPrint <$> S.toList es
  PropertyTestData
    (coerce $ ptName propertyTest)
    failures
    (coerce <$> depMap)

data RuntimeData = RuntimeData
  { rtdName :: Text,
    rtdDescription :: Text,
    rtdMonoType :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

splitTestResults ::
  [TestResult ann] ->
  ( [UnitTest],
    [(PropertyTest, PropertyTestResult ann)]
  )
splitTestResults results =
  let f res = case res of
        PTestResult pt res' -> Right (pt, res')
        UTestResult ut -> Left ut
   in partitionEithers
        ( f
            <$> results
        )

makeTestData ::
  Project Annotation ->
  [TestResult Annotation] ->
  TestData
makeTestData project testResults =
  let (uts, pts) = splitTestResults testResults

      unitTests =
        mkUnitTestData project
          <$> uts

      propertyTests =
        uncurry (mkPropertyTestData project) <$> pts
   in TestData unitTests propertyTests

makeTestDataFromModule :: ModuleTestResults -> TestData
makeTestDataFromModule (ModuleTestResults results) =
  let makeUnitTest (TestName testName, result) = case result of
        ModuleTestPassed -> UnitTestData testName True mempty
        ModuleTestFailed -> UnitTestData testName False mempty
   in TestData (makeUnitTest <$> M.toList results) mempty
