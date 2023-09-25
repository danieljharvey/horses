{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Modules.Types.Test where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import GHC.Generics (Generic)
import Smol.Core.Modules.Types.TestName
import Smol.Core.Types.Constructor
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.TypeName

data Test dep ann
  = UnitTest TestName (Expr dep ann)
  deriving stock (Functor, Generic)

deriving stock instance
  ( Eq ann,
    Eq (dep TypeName),
    Eq (dep Identifier),
    Eq (dep Constructor)
  ) =>
  Eq (Test dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep TypeName),
    Ord (dep Constructor),
    Ord (dep Identifier)
  ) =>
  Ord (Test dep ann)

deriving stock instance
  ( Show ann,
    Show (dep TypeName),
    Show (dep Constructor),
    Show (dep Identifier)
  ) =>
  Show (Test dep ann)

deriving anyclass instance
  ( ToJSONKey (dep Identifier),
    ToJSON ann,
    ToJSON (dep TypeName),
    ToJSON (dep Constructor),
    ToJSON (dep Identifier)
  ) =>
  ToJSON (Test dep ann)

deriving anyclass instance
  ( Ord (dep Identifier),
    Ord (dep Constructor),
    Ord (dep TypeName),
    FromJSONKey (dep Identifier),
    FromJSON ann,
    FromJSON (dep TypeName),
    FromJSON (dep Constructor),
    FromJSON (dep Identifier)
  ) =>
  FromJSON (Test dep ann)
