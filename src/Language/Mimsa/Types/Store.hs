{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Store where

import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Language.Mimsa.Types.Bindings
import Language.Mimsa.Types.ExprHash
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.StoreExpression

-------

-- our environment contains whichever hash/expr pairs we have flapping about
-- and a list of mappings of names to those pieces
data StoreEnv
  = StoreEnv
      { store :: Store,
        bindings :: Bindings
      }
  deriving (Eq, Ord, Show)

instance Semigroup StoreEnv where
  StoreEnv a a' <> StoreEnv b b' = StoreEnv (a <> b) (a' <> b')

instance Monoid StoreEnv where
  mempty = StoreEnv mempty mempty

--------

-- store is where we keep the big map of hashes to expresions
newtype Store = Store {getStore :: Map ExprHash StoreExpression}
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

-- allows us to version our bindings
newtype VersionedBindings
  = VersionedBindings {getVersionedBindings :: Map Name (NonEmpty ExprHash)}
