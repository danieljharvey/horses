{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.Constraint (Constraint (..)) where

import Language.Mimsa.Core

data Constraint
  = ShouldEqual MonoType MonoType
  deriving stock (Eq, Ord, Show)

instance Printer Constraint where
  prettyPrint (ShouldEqual a b) =
    prettyPrint a <> " == " <> prettyPrint b
