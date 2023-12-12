{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smol.Typecheck.Typeclass.ToDictionaryPassing.Types
  ( emptyPassDictEnv,
    emptyPassDictState,
    ToDictEnv (..),
    PassDictEnv (..),
    PassDictState (..),
  )
where

import qualified Data.Map.Strict as M
import Smol.Core.Types
import Smol.Typecheck.Types

data ToDictEnv ann = ToDictEnv
  { tdeClasses :: M.Map TypeclassName (Typeclass ResolvedDep ann),
    tdeInstances :: M.Map (Constraint ResolvedDep ()) (Instance ResolvedDep (Type ResolvedDep ann)),
    tdeVars :: M.Map (ResolvedDep Identifier) ([Constraint ResolvedDep ann], ResolvedType ann)
  }

-- | Are we currently creating an instance? If so, include it's constraint
-- so it is able to refer to itself
newtype PassDictEnv = PassDictEnv
  { pdeCurrentConstraint :: Maybe (Constraint ResolvedDep ())
  }
  deriving newtype (Eq, Ord, Show)

emptyPassDictEnv :: PassDictEnv
emptyPassDictEnv = PassDictEnv Nothing

-- | the instances we've accumulated whilst traversing the expr
newtype PassDictState ann = PassDictState
  { pdsInstances :: M.Map (Constraint ResolvedDep ()) (Expr ResolvedDep (Type ResolvedDep ann))
  }
  deriving newtype (Eq, Ord, Show)

emptyPassDictState :: PassDictState ann
emptyPassDictState = PassDictState mempty
