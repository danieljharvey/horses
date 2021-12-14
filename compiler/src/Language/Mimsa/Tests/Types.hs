{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Tests.Types
  ( Test (..),
    UnitTest (..),
    TestName (..),
    UnitTestSuccess (..),
    PropertyTest (..),
    PropertyTestResult (..),
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
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

newtype UnitTestSuccess = UnitTestSuccess Bool
  deriving newtype
    ( Eq,
      Ord,
      Show,
      JSON.ToJSON,
      JSON.FromJSON,
      ToSchema
    )

data PropertyTestResult var ann
  = PropertyTestSuccess
  | PropertyTestFailures (Set (Expr var ann))
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )
  deriving anyclass
    ( JSON.ToJSON,
      JSON.FromJSON,
      ToSchema
    )

data Test = UTest UnitTest | PTest PropertyTest
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON, ToSchema)

instance Printer Test where
  prettyPrint (PTest pt) = prettyPrint pt
  prettyPrint (UTest ut) = prettyPrint ut

data PropertyTest = PropertyTest
  { ptName :: TestName,
    ptExprHash :: ExprHash,
    ptDeps :: Set ExprHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON, ToSchema)

instance Printer PropertyTest where
  prettyPrint test =
    prettyPrint (ptName test)

data UnitTest = UnitTest
  { utName :: TestName,
    utSuccess :: UnitTestSuccess,
    utExprHash :: ExprHash,
    utDeps :: Set ExprHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON, ToSchema)

instance Printer UnitTest where
  prettyPrint test =
    let tickOrCross = case utSuccess test of
          (UnitTestSuccess True) -> "+++ PASS +++"
          _ -> "--- FAIL ---"
     in tickOrCross <> " " <> prettyPrint (utName test)
