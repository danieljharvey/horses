{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Project.Project where

import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Language.Mimsa.Core
import Language.Mimsa.Types.Project.Versioned
import Language.Mimsa.Types.Store (Store)

-- our environment contains whichever hash/expr pairs we have flapping about
-- and a list of mappings of names to those pieces
data Project ann = Project
  { prjStore :: Store ann,
    prjModules :: VersionedModules,
    prjModuleStore :: Map ModuleHash (Module ann)
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Functor,
      Generic
    )

instance Semigroup (Project a) where
  Project a a1 a2 <> Project b b1 b2 =
    Project (a <> b) (a1 <> b1) (a2 <> b2)

instance Monoid (Project a) where
  mempty = Project mempty mempty mempty

-------------
