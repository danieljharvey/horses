{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Typecheck.Typeclass.Types.Instance
  ( Instance (..),
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import GHC.Generics (Generic)
import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Core.Typecheck.Typeclass.Types.Constraint
import Smol.Core.Types

data Instance dep ann = Instance
  { inConstraints :: [Constraint dep ann],
    inExpr :: Expr dep ann
  }
  deriving stock (Functor, Generic)

deriving stock instance
  ( Eq ann,
    Eq (dep Constructor),
    Eq (dep TypeName),
    Eq (dep Identifier)
  ) =>
  Eq (Instance dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep Constructor),
    Ord (dep TypeName),
    Ord (dep Identifier)
  ) =>
  Ord (Instance dep ann)

deriving stock instance
  ( Show ann,
    Show (dep Constructor),
    Show (dep TypeName),
    Show (dep Identifier)
  ) =>
  Show (Instance dep ann)

deriving anyclass instance
  ( ToJSONKey (dep Identifier),
    ToJSON ann,
    ToJSON (dep Identifier),
    ToJSON (dep Constructor),
    ToJSON (dep TypeName)
  ) =>
  ToJSON (Instance dep ann)

deriving anyclass instance
  ( FromJSON ann,
    FromJSON (dep Constructor),
    FromJSON (dep Identifier),
    FromJSONKey (dep Identifier),
    Ord (dep Identifier),
    FromJSON (dep TypeName)
  ) =>
  FromJSON (Instance dep ann)

instance
  ( Eq (dep Identifier),
    Printer (dep Constructor),
    Printer (dep TypeName),
    Printer (dep Identifier)
  ) =>
  Printer (Instance dep ann)
  where
  prettyDoc (Instance [] expr) = prettyDoc expr
  prettyDoc (Instance constraints expr) =
    "(" <> PP.concatWith (\a b -> a <> ", " <> b) (prettyDoc <$> constraints) <> ") => " <> prettyDoc expr
