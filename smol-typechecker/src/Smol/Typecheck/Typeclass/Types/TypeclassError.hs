{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}

module Smol.Typecheck.Typeclass.Types.TypeclassError
  ( TypeclassError (..),
    Kind (..),
  )
where

import Smol.Core.Types
import Smol.Typecheck.Typeclass.KindChecker

data TypeclassError ann
  = TypeclassNotFound TypeclassName
  | TypeclassInstanceNotFound TypeclassName [Type ResolvedDep ann] [Constraint ResolvedDep ann]
  | ConflictingTypeclassInstancesFound [Constraint ResolvedDep ann]
  | InstanceKindMismatch Identifier Kind Kind -- expected, actual
  deriving stock (Eq, Ord, Show, Foldable)
