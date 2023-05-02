{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.IR.FromExpr.Types
  ( FromExprState (..),
  )
where

import Control.Monad.Identity
import Data.Map.Strict (Map)
import Smol.Core.IR.IRExpr
import qualified Smol.Core.Types as Smol

data FromExprState ann = FromExprState
  { fesModuleParts :: [IRModulePart],
    fesDataTypes :: Map (Identity Smol.TypeName) (Smol.DataType Identity ann),
    fesFreshInt :: Int,
    fesVars :: Map IRIdentifier IRExpr
  }
