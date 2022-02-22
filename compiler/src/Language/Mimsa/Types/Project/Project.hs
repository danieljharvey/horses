{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Project.Project where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import GHC.Generics (Generic)
import Language.Mimsa.Tests.Types
import Language.Mimsa.Types.Project.Versioned
import Language.Mimsa.Types.Store (ExprHash, Store)

-- our environment contains whichever hash/expr pairs we have flapping about
-- and a list of mappings of names to those pieces
data Project ann = Project
  { prjStore :: Store ann,
    prjBindings :: VersionedBindings,
    prjTypeBindings :: VersionedTypeBindings,
    prjTests :: Map ExprHash Test,
    prjOptimised :: Map ExprHash ExprHash -- which SE is an optimisation of another
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Functor,
      Generic
    )
  deriving anyclass
    ( JSON.ToJSON,
      JSON.FromJSON
    )

instance Semigroup (Project a) where
  Project a a1 a2 a3 a4 <> Project b b1 b2 b3 b4 =
    Project (a <> b) (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

instance Monoid (Project a) where
  mempty = Project mempty mempty mempty mempty mempty

-------------
