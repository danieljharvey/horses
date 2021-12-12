{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Tests.Types
  ( Test (..),
    UnitTest (..),
    TestName (..),
    TestSuccess (..),
    PropertyTest (..),
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Printer
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

data Test = UTest UnitTest | PTest PropertyTest
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON, ToSchema)

data PropertyTest = PropertyTest
  { ptName :: TestName,
    ptExprHash :: ExprHash,
    ptDeps :: Set ExprHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON, ToSchema)

data UnitTest = UnitTest
  { utName :: TestName,
    utSuccess :: TestSuccess,
    utExprHash :: ExprHash,
    utDeps :: Set ExprHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON, ToSchema)

instance Printer UnitTest where
  prettyPrint test =
    let tickOrCross = case utSuccess test of
          (TestSuccess True) -> "+++ PASS +++"
          _ -> "--- FAIL ---"
     in tickOrCross <> " " <> prettyPrint (utName test)
