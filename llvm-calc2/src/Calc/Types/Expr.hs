{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}

module Calc.Types.Expr (Expr (..), Op (..)) where

import Calc.Types.Prim

data Expr ann
  = EPrim ann Prim
  | EInfix ann Op (Expr ann) (Expr ann)
  | EIf ann (Expr ann) (Expr ann) (Expr ann)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data Op
  = OpAdd
  | OpMultiply
  | OpSubtract
  | OpEquals
  deriving stock (Eq, Ord, Show)
