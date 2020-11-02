{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Project where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Mimsa.Types.Identifiers (Name, TyCon)
import Language.Mimsa.Types.Store (ExprHash, Store)
import Language.Mimsa.Types.VersionedMap (VersionedMap)

------

newtype ServerUrl = ServerUrl {getServerUrl :: Text}
  deriving (Eq, Ord, Show)
  deriving newtype (JSON.ToJSON, JSON.FromJSON)

-------

-- our environment contains whichever hash/expr pairs we have flapping about
-- and a list of mappings of names to those pieces
data Project ann
  = Project
      { store :: Store ann,
        bindings :: VersionedBindings,
        typeBindings :: VersionedTypeBindings,
        serverUrl :: [ServerUrl]
      }
  deriving (Eq, Ord, Show, Functor)

instance Semigroup (Project a) where
  Project a a1 a2 a3 <> Project b b1 b2 b3 =
    Project (a <> b) (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance Monoid (Project a) where
  mempty = Project mempty mempty mempty mempty

-------------

type VersionedBindings = VersionedMap Name ExprHash

type VersionedTypeBindings = VersionedMap TyCon ExprHash

--------

data SaveProject
  = SaveProject
      { projectVersion :: Int,
        projectBindings :: VersionedBindings,
        projectTypes :: VersionedTypeBindings,
        projectServers :: [ServerUrl]
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)

-----

projectFromSaved :: Store a -> SaveProject -> Project a
projectFromSaved store' sp =
  Project
    { store = store',
      bindings = projectBindings sp,
      typeBindings = projectTypes sp,
      serverUrl = projectServers sp
    }

projectToSaved :: Project a -> SaveProject
projectToSaved proj =
  SaveProject
    { projectVersion = 1,
      projectServers = serverUrl proj,
      projectBindings = bindings proj,
      projectTypes = typeBindings proj
    }
