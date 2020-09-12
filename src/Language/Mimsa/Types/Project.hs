{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Project where

import Data.Text (Text)
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.VersionedMap

------

newtype ServerUrl = ServerUrl {getServerUrl :: Text}
  deriving (Eq, Ord, Show)

-------

-- our environment contains whichever hash/expr pairs we have flapping about
-- and a list of mappings of names to those pieces
data Project
  = Project
      { store :: Store,
        bindings :: VersionedBindings,
        typeBindings :: VersionedTypeBindings,
        serverUrl :: [ServerUrl]
      }
  deriving (Eq, Ord, Show)

instance Semigroup Project where
  Project a a1 a2 a3 <> Project b b1 b2 b3 =
    Project (a <> b) (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance Monoid Project where
  mempty = Project mempty mempty mempty mempty

-------------

type VersionedBindings = VersionedMap Name ExprHash

type VersionedTypeBindings = VersionedMap Construct ExprHash
