{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Project.Project where

import qualified Data.Aeson as JSON
import Data.Swagger
import GHC.Generics (Generic)
import Language.Mimsa.Types.Project.UnitTest
import Language.Mimsa.Types.Project.Versioned
import Language.Mimsa.Types.Store (Store)

-- our environment contains whichever hash/expr pairs we have flapping about
-- and a list of mappings of names to those pieces
data Project ann = Project
  { store :: Store ann,
    bindings :: VersionedBindings,
    typeBindings :: VersionedTypeBindings,
    prjUnitTests :: [UnitTest ann]
  }
  deriving
    ( Eq,
      Ord,
      Show,
      Functor,
      Generic,
      JSON.ToJSON,
      JSON.FromJSON,
      ToSchema
    )

instance Semigroup (Project a) where
  Project a a1 a2 a3 <> Project b b1 b2 b3 =
    Project (a <> b) (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance Monoid (Project a) where
  mempty = Project mempty mempty mempty mempty

-------------
