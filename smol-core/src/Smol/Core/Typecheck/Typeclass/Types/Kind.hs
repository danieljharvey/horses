{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Typecheck.Typeclass.Types.Kind
  ( KindError (..),
    Kind (..),
    UKind (..),
  )
where

import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Core.Types.TypeName

data Kind
  = Star
  | KindFn Kind Kind
  deriving stock (Eq, Ord, Show)

-- | Unresolved Kind
data UKind i
  = UStar
  | UKindFn (UKind i) (UKind i)
  | UVar i
  deriving stock (Eq, Ord, Show)

instance (Show i) => Printer (UKind i) where
  prettyDoc = PP.pretty . show

data KindError dep i
  = KindMismatch (UKind i) (UKind i)
  | UnassignedVar i
  | MissingDataType (dep TypeName)

deriving stock instance
  (Eq i, Eq (dep TypeName)) =>
  Eq (KindError dep i)

deriving stock instance
  (Ord i, Ord (dep TypeName)) =>
  Ord (KindError dep i)

deriving stock instance
  (Show i, Show (dep TypeName)) =>
  Show (KindError dep i)
