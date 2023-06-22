{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Repl.Types
  ( ReplAction (..),
    ReplConfig (..),
  )
where

import GHC.Generics
import Language.Mimsa.Backend.Types
import Language.Mimsa.Core
import Language.Mimsa.Types.Store.RootPath

data ReplAction ann
  = Help
  | Evaluate (Expr Name ann)
  | AddBinding (ModuleItem ann)
  | ListModules (Maybe ModuleName)
  | ListBindings
  | BindModule ModuleName
  | OutputModuleJS (Maybe Backend) ModuleName

data ReplConfig = ReplConfig
  { rcRootPath :: RootPath,
    rcShowLogs :: Bool
  }
  deriving stock (Generic, Eq, Ord, Show)
