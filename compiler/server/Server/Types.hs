module Server.Types where

import qualified Control.Concurrent.STM as STM
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.MimsaConfig
import Language.Mimsa.Types.Store

data MimsaEnvironment = MimsaEnvironment
  { mutableStore :: STM.TVar (Store Annotation),
    mimsaConfig :: MimsaConfig
  }
