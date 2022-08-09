{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Tests
  ( Test (..),
    UnitTest (..),
    TestName (..),
    UnitTestSuccess (..),
    PropertyTest (..),
    PropertyTestResult (..),
    TestResult (..),
    ModuleTestResult (..),
    ModuleTestResults (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Either (partitionEithers)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.OpenApi
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Prettyprinter

newtype TestName = TestName Text
  deriving newtype
    ( Eq,
      Ord,
      Show,
      JSON.ToJSON,
      JSON.FromJSON,
      ToSchema
    )

instance Printer TestName where
  prettyPrint (TestName n) = n

newtype UnitTestSuccess = UnitTestSuccess Bool
  deriving newtype
    ( Eq,
      Ord,
      Show,
      JSON.ToJSON,
      JSON.FromJSON
    )

data PropertyTestResult ann
  = PropertyTestSuccess
  | PropertyTestFailures (Set (Expr Name ann))
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )
  deriving anyclass
    ( JSON.ToJSON,
      JSON.FromJSON
    )

data Test = UTest UnitTest | PTest PropertyTest
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

instance Printer Test where
  prettyPrint (PTest pt) = prettyPrint pt
  prettyPrint (UTest ut) = prettyPrint ut

data PropertyTest = PropertyTest
  { ptName :: TestName,
    ptExprHash :: ExprHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

instance Printer PropertyTest where
  prettyPrint test =
    prettyPrint (ptName test)

data UnitTest = UnitTest
  { utName :: TestName,
    utSuccess :: UnitTestSuccess,
    utExprHash :: ExprHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

instance Printer UnitTest where
  prettyPrint test =
    let tickOrCross = case utSuccess test of
          (UnitTestSuccess True) -> "+++ PASS +++"
          _ -> "--- FAIL ---"
     in tickOrCross <> " " <> prettyPrint (utName test)

data TestResult ann
  = -- | unit test is run once and contains its result
    UTestResult UnitTest
  | -- | property test is effectful and run when returning results
    PTestResult PropertyTest (PropertyTestResult ann)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

instance Printer (TestResult ann) where
  prettyPrint (UTestResult ut) = prettyPrint ut
  prettyPrint (PTestResult pt ptRes) =
    let tickOrCross = case ptRes of
          PropertyTestSuccess -> "+++ PASS +++"
          _ -> "--- FAIL ---"
        failures = case ptRes of
          PropertyTestSuccess -> ""
          PropertyTestFailures es ->
            "\nFailing inputs:\n" <> T.intercalate "\n" ((<>) " - " . prettyPrint <$> S.toList es)
     in tickOrCross <> " " <> prettyPrint pt <> failures

newtype ModuleTestResults = ModuleTestResults {getModuleTests :: Map TestName ModuleTestResult}
  deriving newtype (Eq, Ord, Show)

instance Printer ModuleTestResults where
  prettyDoc (ModuleTestResults results) | M.null results = ""
  prettyDoc results =
    case partitionModuleTestResults results of
      ([], successes) -> "👍" <+> pretty (length successes) <+> "tests passed"
      (fails, successes) ->
        let printFail testName = "💩" <+> prettyDoc testName <> line
            printSuccess testName = "👍" <+> prettyDoc testName <> line
            successCount = pretty (length successes) <> "\\" <> pretty (length successes + length fails) <+> "tests passed"
         in successCount <> line <> foldMap printFail fails <> foldMap printSuccess successes

-- | split the test results into a list of successes and failures
-- when we come to include property tests this will need to show
-- failed cases too
partitionModuleTestResults :: ModuleTestResults -> ([TestName], [TestName])
partitionModuleTestResults (ModuleTestResults results) =
  partitionEithers $
    ( \(testName, result) -> case result of
        ModuleTestPassed -> Right testName
        ModuleTestFailed -> Left testName
    )
      <$> M.toList results

data ModuleTestResult
  = ModuleTestPassed
  | ModuleTestFailed
  deriving stock (Eq, Ord, Show)

instance Printer ModuleTestResult where
  prettyPrint ModuleTestPassed = "+"
  prettyPrint ModuleTestFailed = "-"
