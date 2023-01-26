{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.IR.FromExpr.Types
  ( FromExprState (..),
  )
where

import Data.Map.Strict (Map)
import Smol.Core.IR.IRExpr
import qualified Smol.Core.Types as Smol

import Control.Monad.Identity

data FromExprState ann = FromExprState
  { fesModuleParts :: [IRModulePart],
    dataTypes :: Map Smol.TypeName (Smol.DataType Identity ann),
    freshInt :: Int,
    vars :: Map IRIdentifier IRExpr
  }
