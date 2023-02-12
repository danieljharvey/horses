{-# LANGUAGE DerivingStrategies #-}
module Calc.Typecheck.Elaborate (elaborate) where

import Calc.Types.Expr
import Calc.Types.Type

data TypeError ann = OhNo
  deriving stock (Eq,Ord,Show)

elaborate :: Expr ann -> Either (TypeError ann) (Expr (Type ann))
elaborate = undefined


