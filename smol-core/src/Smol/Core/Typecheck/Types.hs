{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Smol.Core.Typecheck.Types
  ( TCEnv (..),
    Typeclass (..),
    TypeclassHead (..),
    Instance (..),
    module Smol.Core.Typecheck.Types.TCError,
    module Smol.Core.Typecheck.Types.TCState,
    module Smol.Core.Typecheck.Types.TCWrite,
  )
where

import Control.Monad.Identity
import Data.Map.Strict (Map)
import Smol.Core.Typecheck.Types.TCError
import Smol.Core.Typecheck.Types.TCState
import Smol.Core.Typecheck.Types.TCWrite
import Smol.Core.Types

data Typeclass ann = Typeclass
  { tcName :: String,
    tcArgs :: [Identifier],
    tcFuncName :: Identifier,
    tcFuncType :: Type Identity ann
  }

data TypeclassHead ann
  = TypeclassHead String [Type Identity ann]
  deriving stock (Eq, Ord, Show)

data Instance ann = Instance
  {inExpr :: Expr Identity ann}
  deriving stock (Eq, Ord, Show)

data TCEnv ann = TCEnv
  { tceVars :: Map (ResolvedDep Identifier) (ResolvedType ann),
    tceGlobals :: Map Identifier (ResolvedType ann),
    tceDataTypes :: Map (ResolvedDep TypeName) (DataType ResolvedDep ann),
    tceClasses :: Map String (Typeclass ann),
    tceInstances :: Map (TypeclassHead ann) (Instance ann)
  }
