{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Types.Constraint
  ( Constraint (..),
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import GHC.Generics (Generic)
import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Core.Types.Constructor
import Smol.Core.Types.Identifier
import Smol.Core.Types.Type
import Smol.Core.Types.TypeName
import Smol.Core.Types.TypeclassName

data Constraint dep ann = Constraint
  { conTypeclass :: TypeclassName,
    conType :: [Type dep ann]
  }
  deriving stock (Functor, Foldable, Generic)

deriving stock instance
  ( Eq ann,
    Eq (dep Constructor),
    Eq (dep TypeName),
    Eq (dep Identifier)
  ) =>
  Eq (Constraint dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep Constructor),
    Ord (dep TypeName),
    Ord (dep Identifier)
  ) =>
  Ord (Constraint dep ann)

deriving stock instance
  ( Show ann,
    Show (dep Constructor),
    Show (dep TypeName),
    Show (dep Identifier)
  ) =>
  Show (Constraint dep ann)

deriving anyclass instance
  ( ToJSONKey (dep Identifier),
    ToJSON ann,
    ToJSON (dep Identifier),
    ToJSON (dep Constructor),
    ToJSON (dep TypeName)
  ) =>
  ToJSON (Constraint dep ann)

deriving anyclass instance
  ( ToJSONKey (dep Identifier),
    ToJSON ann,
    ToJSON (dep Identifier),
    ToJSON (dep Constructor),
    ToJSON (dep TypeName)
  ) =>
  ToJSONKey (Constraint dep ann)

deriving anyclass instance
  ( FromJSON ann,
    FromJSON (dep Constructor),
    FromJSON (dep Identifier),
    FromJSONKey (dep Identifier),
    Ord (dep Identifier),
    FromJSON (dep TypeName)
  ) =>
  FromJSON (Constraint dep ann)

deriving anyclass instance
  ( FromJSON ann,
    FromJSON (dep Constructor),
    FromJSON (dep Identifier),
    FromJSONKey (dep Identifier),
    FromJSON (dep TypeName),
    Ord (dep Constructor),
    Ord (dep Identifier)
  ) =>
  FromJSONKey (Constraint dep ann)

instance
  ( Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Printer (Constraint dep ann)
  where
  prettyDoc (Constraint tcn tys) =
    prettyDoc tcn
      PP.<+> PP.concatWith
        (\a b -> a <> " " <> b)
        (prettyDoc <$> tys)
