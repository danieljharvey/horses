module Calc.Types.Module where

import Calc.Types.Function
import Data.Map.Strict (Map)

newtype Module ann =
  Module { mdFunctions :: Map FunctionName (Function ann) }
