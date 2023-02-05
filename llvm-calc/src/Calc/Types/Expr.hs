module Calc.Types.Expr (Expr(..), Op(..)) where

data Expr ann =
    EPrim ann Int
  | EInfix ann Op (Expr ann) (Expr ann)

data Op = Add | Multiply | Subtract | Equals

