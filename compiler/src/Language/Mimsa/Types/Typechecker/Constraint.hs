{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.Constraint (Constraint (..)) where

import Language.Mimsa.Printer
import Language.Mimsa.Types.Typechecker.MonoType

data Constraint = ShouldEqual MonoType MonoType
  deriving stock (Eq, Ord, Show)

instance Printer Constraint where
  prettyPrint (ShouldEqual a b) =
    prettyPrint a <> " == " <> prettyPrint b
