{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Repl.Types
  ( ReplAction (..),
    ReplConfig (..),
  )
where

import GHC.Generics
import Language.Mimsa.Backend.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.RootPath
import Language.Mimsa.Types.Tests
import Language.Mimsa.Types.Typechecker

data ReplAction ann
  = Help
  | Info (Expr Name ann)
  | Evaluate (Expr Name ann)
  | Bind Name (Expr Name ann)
  | OutputJS (Maybe Backend) (Expr Name ann)
  | TypeSearch MonoType
  | BindType DataType
  | ListBindings
  | AddUnitTest TestName (Expr Name ann)
  | ListTests (Maybe Name)

data ReplConfig = ReplConfig
  { rcRootPath :: RootPath,
    rcShowLogs :: Bool
  }
  deriving stock (Generic, Eq, Ord, Show)
