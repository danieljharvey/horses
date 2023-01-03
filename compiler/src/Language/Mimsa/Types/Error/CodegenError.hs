{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.CodegenError where

import Language.Mimsa.Core
import Prettyprinter

data CodegenError
  = TypeShouldHaveAtLeastOneVariable
  | TypeShouldHaveNoVariables
  | ExpectedNonEmptyMap
  | NoConstructorMatches
  | TooManyConstructorMatches
  | CannotUseNonFunctorValue
  | UnknownCodegenError -- for Monoid
  | Multiple CodegenError CodegenError
  | CouldNotFindVarsInType
  | RecursingOverAnotherType
  | MultipleFunctorVariablesInApplicativeArg
  | NewtypeShouldOnlyHaveOneArgument
  | ConstructorShouldHaveNoArgs TyCon
  deriving stock (Eq, Show)

instance Semigroup CodegenError where
  a <> b = Multiple a b

instance Monoid CodegenError where
  mempty = UnknownCodegenError

instance Printer CodegenError where
  prettyDoc TypeShouldHaveAtLeastOneVariable = "Type should have at least one type variable"
  prettyDoc TypeShouldHaveNoVariables = "Type should have no variables"
  prettyDoc ExpectedNonEmptyMap = "Expected non-empty map"
  prettyDoc NoConstructorMatches = "No constructor matches"
  prettyDoc TooManyConstructorMatches = "Too many constructor matches"
  prettyDoc CannotUseNonFunctorValue = "Cannot use non-functor value"
  prettyDoc UnknownCodegenError = "UnknownCodegenError"
  prettyDoc CouldNotFindVarsInType = "Could not find vars in type"
  prettyDoc RecursingOverAnotherType = "Cannot recurse over another data type"
  prettyDoc MultipleFunctorVariablesInApplicativeArg = "Multiple functor variables in first applicative argument"
  prettyDoc NewtypeShouldOnlyHaveOneArgument = "Newtype should only have one type argument"
  prettyDoc (ConstructorShouldHaveNoArgs tyCon) = "Constructor" <+> prettyDoc tyCon <+> "should have no arguments"
  prettyDoc (Multiple a b) = prettyDoc a <> line <> prettyDoc b
