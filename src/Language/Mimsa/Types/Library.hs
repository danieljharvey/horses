module Language.Mimsa.Types.Library where

import Data.Map (Map)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.ForeignFunc

-- our built-in functions for doing IO things etc
-- statically defined and made available in all computations for now
newtype Library = Library {getLibrary :: Map FuncName ForeignFunc}
