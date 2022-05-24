module Language.Mimsa.Modules.HashModule (hashModule) where

import Control.Monad (void)
import Data.Coerce
import Language.Mimsa.Store.Hashing
import Language.Mimsa.Types.Modules.Module
import Language.Mimsa.Types.Modules.ModuleHash
import Language.Mimsa.Types.Project.ProjectHash

-- we remove annotations before producing the hash
-- so formatting does not affect it
hashModule :: Module ann -> ModuleHash
hashModule = coerce . snd . contentAndHash . void
