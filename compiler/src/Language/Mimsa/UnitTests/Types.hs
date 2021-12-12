{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.UnitTests.Types where

import qualified Data.Aeson as JSON
import Data.OpenApi
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

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

newtype TestSuccess = TestSuccess Bool
  deriving newtype
    ( Eq,
      Ord,
      Show,
      JSON.ToJSON,
      JSON.FromJSON,
      ToSchema
    )

data PropertyTestResult
  = PropertyTestSuccess
  | PropertyTestFailures (Set (Expr Name ()))
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON, ToSchema)

data UnitTest
  = UnitTest
      { utName :: TestName,
        utSuccess :: TestSuccess,
        utExprHash :: ExprHash,
        utDeps :: Set ExprHash
      }
  | PropertyTest
      { ptName :: TestName,
        ptResult :: PropertyTestResult,
        ptExprHash :: ExprHash,
        ptDeps :: Set ExprHash
      }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON, ToSchema)

instance Printer UnitTest where
  prettyPrint test =
    let tickOrCross = case utSuccess test of
          (TestSuccess True) -> "+++ PASS +++"
          _ -> "--- FAIL ---"
     in tickOrCross <> " " <> prettyPrint (utName test)
