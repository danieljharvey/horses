module Server.Types where

import qualified Control.Concurrent.STM as STM
import Data.Map.Strict (Map)
import Language.Mimsa.Core
import Language.Mimsa.Types.Store
import Server.ServerConfig

data MimsaEnvironment = MimsaEnvironment
  { mutableStore :: STM.TVar (Store Annotation),
    mutableModuleStore :: STM.TVar (Map ModuleHash (Module Annotation)),
    mimsaConfig :: ServerConfig
  }
