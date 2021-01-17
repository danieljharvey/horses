{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.Mimsa.Types.Project.UnitTest where

import qualified Data.Aeson as JSON
import Data.Set (Set)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics
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

newtype TestSuccess = TestSuccess Bool
  deriving newtype
    ( Eq,
      Ord,
      Show,
      JSON.ToJSON,
      JSON.FromJSON,
      ToSchema
    )

data UnitTest ann = UnitTest
  { utName :: TestName,
    utSuccess :: TestSuccess,
    utExpr :: Expr Name ann,
    utDeps :: Set ExprHash
  }
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON, ToSchema)
