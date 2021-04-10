module Language.Mimsa.Types.Swaps where

import Data.Map (Map)
import Language.Mimsa.Types.Identifiers

-- the names that get changed in substitution
-- Name is the original name
-- Variable is the new variable
-- TODO: shouldn't all swaps be to numbered now?
type Swaps = Map Variable Name
