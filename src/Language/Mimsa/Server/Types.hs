module Language.Mimsa.Server.Types where

import qualified Control.Concurrent.STM as STM
import Language.Mimsa.Server.EnvVars
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Store

data MimsaEnvironment = MimsaEnvironment
  { mutableStore :: STM.TVar (Store Annotation),
    mimsaConfig :: MimsaConfig
  }
