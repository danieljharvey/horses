{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Project.SaveProject where

import Control.Monad (mzero)
import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import Language.Mimsa.Types.Project.Versioned

data SaveProject = SaveProject
  { projectVersion :: Int,
    projectBindings :: VersionedBindings,
    projectTypes :: VersionedTypeBindings
  }
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON)

instance JSON.FromJSON SaveProject where
  parseJSON (JSON.Object o) = do
    version <- o .: "projectVersion"
    bindings <- o .: "projectBindings"
    types <- o .: "projectTypes"
    SaveProject
      <$> JSON.parseJSON version
      <*> JSON.parseJSON bindings
      <*> JSON.parseJSON types
  parseJSON _ = mzero
