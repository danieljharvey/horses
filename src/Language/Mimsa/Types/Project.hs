{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Project where

import qualified Data.Aeson as JSON
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Store

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
        serverUrl :: [ServerUrl]
      }
  deriving (Eq, Ord, Show)

instance Semigroup Project where
  Project a a1 a2 <> Project b b1 b2 =
    Project (a <> b) (a1 <> b1) (a2 <> b2)

instance Monoid Project where
  mempty = Project mempty mempty mempty

-------------

-- allows us to version our bindings
newtype VersionedBindings
  = VersionedBindings {getVersionedBindings :: Map Name (NonEmpty ExprHash)}
  deriving newtype (Eq, Ord, Show, Monoid)
  deriving (JSON.ToJSON, JSON.FromJSON)

instance Semigroup VersionedBindings where
  (VersionedBindings a) <> (VersionedBindings b) =
    VersionedBindings (M.unionWith combineUnique a b)

-- we don't want duplicates in list
-- nub keeps first instance, we want last instance, hence the reversing
combineUnique :: (Eq a) => NonEmpty a -> NonEmpty a -> NonEmpty a
combineUnique as bs =
  let as' = NE.toList as
      bs' = NE.toList bs
   in NE.fromList . reverse . nub . reverse $ as' <> bs'
