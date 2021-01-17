{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Mimsa.Types.Project.UnitTest where

import qualified Data.Aeson as JSON
import Data.Set (Set)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
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

newtype TestSuccess = TestSuccess Bool
  deriving newtype
    ( Eq,
      Ord,
      Show,
      JSON.ToJSON,
      JSON.FromJSON,
      ToSchema
    )

data UnitTest = UnitTest
  { utName :: TestName,
    utSuccess :: TestSuccess,
    utExprHash :: ExprHash,
    utDeps :: Set ExprHash
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON, ToSchema)
