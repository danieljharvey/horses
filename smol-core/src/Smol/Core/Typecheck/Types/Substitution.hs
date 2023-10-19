{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Typecheck.Types.Substitution
  ( Substitution (..),
    SubstitutionMatcher (..),
  )
where

import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Core.Types

data SubstitutionMatcher dep ann
  = SubId (dep Identifier)
  | SubUnknown Integer
  | SubType (Type dep ann)

deriving stock instance
  ( Eq ann,
    Eq (dep Identifier),
    Eq (dep TypeName)
  ) =>
  Eq (SubstitutionMatcher dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep Identifier),
    Ord (dep TypeName)
  ) =>
  Ord (SubstitutionMatcher dep ann)

deriving stock instance
  ( Show ann,
    Show (dep Identifier),
    Show (dep TypeName)
  ) =>
  Show (SubstitutionMatcher dep ann)

---------------------

data Substitution dep ann
  = Substitution (SubstitutionMatcher dep ann) (Type dep ann)

instance (Show ann, Show (dep Identifier), Show (dep TypeName)) => Printer (Substitution dep ann) where
  prettyDoc a = PP.pretty (show a)

deriving stock instance
  (Eq ann, Eq (dep Identifier), Eq (dep TypeName)) =>
  Eq (Substitution dep ann)

deriving stock instance
  (Ord ann, Ord (dep Identifier), Ord (dep TypeName)) =>
  Ord (Substitution dep ann)

deriving stock instance
  (Show ann, Show (dep Identifier), Show (dep TypeName)) =>
  Show (Substitution dep ann)
