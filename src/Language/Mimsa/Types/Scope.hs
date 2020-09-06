{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Scope where

import Data.Map (Map)
import Language.Mimsa.Types.Expr
import Language.Mimsa.Types.Variable

-- dependencies resolved into actual expressions
newtype Scope = Scope {getScope :: Map Variable (Expr Variable)}
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)
