module Language.Mimsa.Types.Swaps where

import Data.Map (Map)
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Variable

-- the names that get changed in substitution
type Swaps = Map Variable Name
