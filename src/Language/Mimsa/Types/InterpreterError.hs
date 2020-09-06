module Language.Mimsa.Types.InterpreterError where

import Data.Map (Map)
import Language.Mimsa.Types.Expr
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Variable

data InterpreterError
  = UnknownInterpreterError
  | CouldNotFindVar Scope Variable
  | CouldNotFindBuiltIn Scope Variable
  | CannotDestructureAsPair (Expr Variable)
  | CannotDestructureAsSum (Expr Variable)
  | CannotDestructureAsRecord (Expr Variable) Name
  | CannotDestructureAsList (Expr Variable)
  | CannotApplyToNonFunction (Expr Variable)
  | CannotFindMemberInRecord (Map Name (Expr Variable)) Name
  | PredicateForIfMustBeABoolean (Expr Variable)
  | CouldNotUnwrapBuiltIn Variable
  | CouldNotMatchBuiltInId BiIds
  | PatternMatchFailure (Expr Variable)
  | SelfReferencingBinding Variable
  deriving (Eq, Ord, Show)

instance Semigroup InterpreterError where
  a <> _ = a

instance Monoid InterpreterError where
  mempty = UnknownInterpreterError
