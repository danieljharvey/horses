{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Types.Project.SaveProject where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Aeson as JSON
import Data.Map (Map)
import GHC.Generics (Generic)
import Language.Mimsa.Types.Project.Versioned
import Language.Mimsa.Types.Store
import Language.Mimsa.UnitTests.Types

data SaveProject = SaveProject
  { projectVersion :: Int,
    projectBindings :: VersionedBindings,
    projectTypes :: VersionedTypeBindings,
    projectUnitTests :: Map ExprHash UnitTest
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON)

instance JSON.FromJSON SaveProject where
  parseJSON (JSON.Object o) = do
    version <- o .: "projectVersion"
    bindings <- o .: "projectBindings"
    types <- o .: "projectTypes"
    unitTests <- o .:? "projectUnitTests"
    tests <- case unitTests of
      Just as -> JSON.parseJSON as
      Nothing -> pure mempty
    SaveProject
      <$> JSON.parseJSON version
      <*> JSON.parseJSON bindings
      <*> JSON.parseJSON types
      <*> pure tests
  parseJSON _ = mzero
