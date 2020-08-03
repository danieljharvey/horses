{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Project where

import qualified Data.Aeson as JSON
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Store

-------

-- our environment contains whichever hash/expr pairs we have flapping about
-- and a list of mappings of names to those pieces
data Project
  = Project
      { store :: Store,
        bindings :: VersionedBindings
      }
  deriving (Eq, Ord, Show)

instance Semigroup Project where
  Project a a' <> Project b b' = Project (a <> b) (a' <> b')

instance Monoid Project where
  mempty = Project mempty mempty

-------------

-- allows us to version our bindings
newtype VersionedBindings
  = VersionedBindings {getVersionedBindings :: Map Name (NonEmpty ExprHash)}
  deriving newtype (Eq, Ord, Show, Monoid)
  deriving (JSON.ToJSON, JSON.FromJSON)

instance Semigroup VersionedBindings where
  (VersionedBindings a) <> (VersionedBindings b) =
    VersionedBindings (M.unionWith (<>) a b)
