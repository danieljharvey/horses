{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module ReplNew.Types
  ( ReplAction (..),
    ReplConfig (..),
  )
where

import GHC.Generics
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Store.RootPath

data ReplAction ann
  = Help
  | Evaluate (Expr Name ann)
  | AddBinding (ModuleItem ann)
  | ListModules (Maybe ModuleName)
  | ListBindings

data ReplConfig = ReplConfig
  { rcRootPath :: RootPath,
    rcShowLogs :: Bool
  }
  deriving stock (Generic, Eq, Ord, Show)
