{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Typecheck.Typeclass.Types
  ( Typeclass (..),
    module Smol.Core.Typecheck.Typeclass.Types.Constraint,
    module Smol.Core.Typecheck.Typeclass.Types.Instance,
    module Smol.Core.Typecheck.Typeclass.Types.TypeclassName,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import GHC.Generics (Generic)
import Smol.Core.Typecheck.Typeclass.Types.Constraint
import Smol.Core.Typecheck.Typeclass.Types.Instance
import Smol.Core.Typecheck.Typeclass.Types.TypeclassName
import Smol.Core.Types

-- | the typeclass described in it's most general form, ie
-- class Show a where show :: a -> String
data Typeclass dep ann = Typeclass
  { tcName :: TypeclassName,
    tcArgs :: [Identifier],
    tcFuncName :: Identifier,
    tcFuncType :: Type dep ann
  }
  deriving stock (Functor, Generic)

deriving stock instance
  ( Eq ann,
    Eq (dep Constructor),
    Eq (dep TypeName),
    Eq (dep Identifier)
  ) =>
  Eq (Typeclass dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep Constructor),
    Ord (dep TypeName),
    Ord (dep Identifier)
  ) =>
  Ord (Typeclass dep ann)

deriving stock instance
  ( Show ann,
    Show (dep Constructor),
    Show (dep TypeName),
    Show (dep Identifier)
  ) =>
  Show (Typeclass dep ann)

deriving anyclass instance
  ( ToJSONKey (dep Identifier),
    ToJSON ann,
    ToJSON (dep Identifier),
    ToJSON (dep Constructor),
    ToJSON (dep TypeName)
  ) =>
  ToJSON (Typeclass dep ann)

deriving anyclass instance
  ( FromJSON ann,
    FromJSON (dep Constructor),
    FromJSON (dep Identifier),
    FromJSONKey (dep Identifier),
    Ord (dep Identifier),
    FromJSON (dep TypeName)
  ) =>
  FromJSON (Typeclass dep ann)
