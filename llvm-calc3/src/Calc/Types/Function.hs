module Calc.Types.Function (ArgumentName(..), FunctionName(..), Function(..)) where

import Calc.Types.Expr
import Calc.Types.Type
import Data.Text (Text)

newtype FunctionName = FunctionName Text

newtype ArgumentName = ArgumentName Text

data Function ann = Function
  { fnArgs :: [(ArgumentName, Type ann)],
    fnReturnType :: Type ann,
    fnBody :: Expr ann
  }
