{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Project.SaveProject where

import qualified Data.Aeson as JSON
import GHC.Generics (Generic)
import Language.Mimsa.Types.Project.Versioned

data SaveProject
  = SaveProject
      { projectVersion :: Int,
        projectBindings :: VersionedBindings,
        projectTypes :: VersionedTypeBindings
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)
