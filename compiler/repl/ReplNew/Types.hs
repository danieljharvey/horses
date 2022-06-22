{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module ReplNew.Types
  ( ReplAction (..),
    ReplConfig (..),
  )
where

import GHC.Generics
import Language.Mimsa.Backend.Types
import Language.Mimsa.Tests.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.RootPath
import Language.Mimsa.Types.Typechecker

data ReplAction ann
  = Help
  | Evaluate (Expr Name ann)
  | Tree (Expr Name ann)
  | Graph (Expr Name ann)
  | ProjectGraph
  | Bind Name (Expr Name ann)
  | OutputJS (Maybe Backend) (Expr Name ann)
  | TypeSearch MonoType
  | BindType DataType
  | ListModules
  | AddUnitTest TestName (Expr Name ann)
  | ListTests (Maybe Name)
  | Upgrade Name
  | Optimise Name

data ReplConfig = ReplConfig
  { rcRootPath :: RootPath,
    rcShowLogs :: Bool
  }
  deriving stock (Generic, Eq, Ord, Show)
