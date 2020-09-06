module Language.Mimsa.Types.Library where

import Data.Map (Map)
import Language.Mimsa.Types.ForeignFunc
import Language.Mimsa.Types.FuncName

-- our built-in functions for doing IO things etc
-- statically defined and made available in all computations for now
newtype Library = Library {getLibrary :: Map FuncName ForeignFunc}
