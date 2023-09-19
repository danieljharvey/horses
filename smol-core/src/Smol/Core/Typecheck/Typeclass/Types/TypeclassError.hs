{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}

module Smol.Core.Typecheck.Typeclass.Types.TypeclassError
  ( TypeclassError (..),
    Kind (..),
  )
where

import Smol.Core.Typecheck.Typeclass.KindChecker
import Smol.Core.Typecheck.Typeclass.Types
import Smol.Core.Types

data TypeclassError ann
  = TypeclassNotFound TypeclassName
  | TypeclassInstanceNotFound TypeclassName [Type ResolvedDep ann] [Constraint ResolvedDep ann]
  | ConflictingTypeclassInstancesFound [Constraint ResolvedDep ann]
  | KindMismatch Identifier Kind Kind -- expected, actual
  deriving stock (Eq, Ord, Show, Foldable)
