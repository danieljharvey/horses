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
import qualified Data.Map.Strict as M
import Data.OpenApi hiding (get)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Tests

data TestData = TestData
  { tdUnitTests :: [UnitTestData],
    tdPropertyTests :: [PropertyTestData]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

data UnitTestData = UnitTestData
  { utdTestName :: Text,
    utdTestSuccess :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

mkUnitTestData :: UnitTest -> UnitTestData
mkUnitTestData unitTest =
  UnitTestData
    (coerce $ utName unitTest)
    (coerce $ utSuccess unitTest)

data PropertyTestData = PropertyTestData
  { ptdTestName :: Text,
    ptdTestFailures :: [Text]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

mkPropertyTestData ::
  PropertyTest ->
  PropertyTestResult ann ->
  PropertyTestData
mkPropertyTestData propertyTest result = do
  let failures = case result of
        PropertyTestSuccess -> mempty
        PropertyTestFailures es -> prettyPrint <$> S.toList es
  PropertyTestData
    (coerce $ ptName propertyTest)
    failures

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
  [TestResult Annotation] ->
  TestData
makeTestData testResults =
  let (uts, pts) = splitTestResults testResults

      unitTests =
        mkUnitTestData
          <$> uts

      propertyTests =
        uncurry mkPropertyTestData <$> pts
   in TestData unitTests propertyTests

makeTestDataFromModule :: ModuleTestResults -> TestData
makeTestDataFromModule (ModuleTestResults results) =
  let makeUnitTest (TestName testName, result) = case result of
        ModuleTestPassed -> UnitTestData testName True
        ModuleTestFailed -> UnitTestData testName False
   in TestData (makeUnitTest <$> M.toList results) mempty
