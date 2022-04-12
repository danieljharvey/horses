module Server.Types where

import qualified Control.Concurrent.STM as STM
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Store
import Server.ServerConfig

data MimsaEnvironment = MimsaEnvironment
  { mutableStore :: STM.TVar (Store Annotation),
    mimsaConfig :: ServerConfig
  }
