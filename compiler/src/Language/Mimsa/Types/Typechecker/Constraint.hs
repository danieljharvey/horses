{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.Constraint (Constraint (..)) where

import Language.Mimsa.Printer
import Language.Mimsa.Types.Typechecker.MonoType
import Language.Mimsa.Types.Typechecker.Scheme

data Constraint
  = ShouldEqual MonoType MonoType
  | InstanceOf MonoType Scheme
  deriving stock (Eq, Ord, Show)

instance Printer Constraint where
  prettyPrint (ShouldEqual a b) =
    prettyPrint a <> " == " <> prettyPrint b
  prettyPrint (InstanceOf a b) =
    prettyPrint a <> " is an instance of " <> prettyPrint b
