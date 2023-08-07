{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Types
  ( TCEnv (..),
    module Smol.Core.Typecheck.Typeclass.Types,
    module Smol.Core.Typecheck.Types.TCError,
    module Smol.Core.Typecheck.Types.TCState,
    module Smol.Core.Typecheck.Types.TCWrite,
  )
where

import Data.Map.Strict (Map)
import Smol.Core.Typecheck.Typeclass.Types
import Smol.Core.Typecheck.Types.TCError
import Smol.Core.Typecheck.Types.TCState
import Smol.Core.Typecheck.Types.TCWrite
import Smol.Core.Types

data TCEnv ann = TCEnv
  { tceVars :: Map (ResolvedDep Identifier) ([Constraint ann], ResolvedType ann),
    tceDataTypes :: Map (ResolvedDep TypeName) (DataType ResolvedDep ann),
    tceClasses :: Map TypeclassName (Typeclass ann),
    tceInstances :: Map (Constraint ann) (Instance ann),
    tceConstraints :: [Constraint ann]
  }
