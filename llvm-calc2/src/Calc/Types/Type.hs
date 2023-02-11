module Calc.Types.Type (Type(..)) where

data TypePrim = TBool | TInt

data Type ann =
  TPrim ann TypePrim
