module Language.Mimsa.Types.TypeError
  ( TypeError (..),
  )
where

import Data.Map (Map)
import Data.Set (Set)
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.Environment
import Language.Mimsa.Types.Expr
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Variable

data TypeError
  = UnknownTypeError
  | FailsOccursCheck Swaps Variable MonoType
  | UnificationError MonoType MonoType
  | VariableNotInEnv Swaps Variable (Set Variable)
  | MissingRecordMember Name (Set Name)
  | MissingRecordTypeMember Name (Map Name MonoType)
  | MissingBuiltIn Variable
  | CannotUnifyBoundVariable Variable MonoType
  | CannotMatchRecord Environment MonoType
  | CaseMatchExpectedPair MonoType
  | CannotCaseMatchOnType (Expr Variable)
  | TypeConstructorNotInScope Environment Construct
  | TypeIsNotConstructor (Expr Variable)
  | TypeVariableNotInDataType Construct Name [Name]
  | ConflictingConstructors Construct
  | CannotApplyToType Construct
  | DuplicateTypeDeclaration Construct
  | IncompletePatternMatch [Construct]
  | MixedUpPatterns [Construct]
  deriving (Eq, Ord, Show)

instance Semigroup TypeError where
  a <> _ = a

instance Monoid TypeError where
  mempty = UnknownTypeError
