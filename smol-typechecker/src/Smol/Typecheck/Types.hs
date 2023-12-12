module Smol.Typecheck.Types
  ( TCEnv (..),
    module Smol.Typecheck.Typeclass.Types,
    module Smol.Typecheck.Types.TCError,
    module Smol.Typecheck.Types.TCState,
    module Smol.Typecheck.Types.TCWrite,
  )
where

import Data.Map.Strict (Map)
import Smol.Core.Types
import Smol.Typecheck.Typeclass.Types
import Smol.Typecheck.Types.TCError
import Smol.Typecheck.Types.TCState
import Smol.Typecheck.Types.TCWrite

data TCEnv ann = TCEnv
  { tceVars :: Map (ResolvedDep Identifier) ([Constraint ResolvedDep ann], ResolvedType ann),
    tceDataTypes :: Map (ResolvedDep TypeName) (DataType ResolvedDep ann),
    tceClasses :: Map TypeclassName (Typeclass ResolvedDep ann),
    tceInstances :: Map (Constraint ResolvedDep ann) (Instance ResolvedDep ann),
    tceConstraints :: [Constraint ResolvedDep ann]
  }
