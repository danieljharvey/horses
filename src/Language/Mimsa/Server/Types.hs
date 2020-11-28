module Language.Mimsa.Server.Types where

import Data.IORef
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Store

newtype MimsaEnvironment
  = MimsaEnvironment {mutableStore :: IORef (Store Annotation)}
