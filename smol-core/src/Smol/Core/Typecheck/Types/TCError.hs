{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}

module Smol.Core.Typecheck.Types.TCError
  ( TCError (..),
  )
where

import Data.Set (Set)
import Smol.Core.Typecheck.Typeclass.Types
import Smol.Core.Types
import Smol.Core.Types.PatternMatchError (PatternMatchError)

data TCError ann
  = TCUnknownError
  | TCCouldNotFindVar ann (ResolvedDep Identifier)
  | TCTypeMismatch (ResolvedType ann) (ResolvedType ann)
  | TCTupleSizeMismatch Int (ResolvedType ann)
  | TCExpectedTuple (ResolvedType ann)
  | TCExpectedFunction (ResolvedType ann)
  | TCRecordMissingItems (Set Identifier)
  | TCExpectedRecord (ResolvedType ann)
  | TCInfixMismatch Op (ResolvedType ann) (ResolvedType ann)
  | TCPatternMismatch (Pattern ResolvedDep ann) (ResolvedType ann)
  | TCUnknownConstructor (ResolvedDep Constructor) [Constructor]
  | TCConstructorArgumentMismatch (ResolvedDep Constructor) Int Int -- expected, actual
  | TCExpectedConstructorType (ResolvedType ann)
  | TCCompoundTypeInEquality (ResolvedType ann) -- for now we only do primitive equality
  | TCPatternMatchError (PatternMatchError (ResolvedType ann))
  | TCTypeclassNotFound TypeclassName
  | TCTypeclassInstanceNotFound TypeclassName [Type ResolvedDep ann] [Constraint ResolvedDep ann]
  | TCConflictingTypeclassInstancesFound [Constraint ResolvedDep ann]
  deriving stock (Eq, Ord, Show, Foldable)
