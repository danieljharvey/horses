module Calc.Types.Module where

import Calc.Types.Function
import Calc.Types.Expr

data Module ann =
    Module {mdFunctions :: [Function ann],
      mdExpr :: Expr ann}
