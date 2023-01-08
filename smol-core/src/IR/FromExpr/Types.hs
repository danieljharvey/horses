{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module IR.FromExpr.Types
  ( FromExprState (..),
  )
where

import Data.Map.Strict (Map)
import IR.IRExpr
import qualified Types as Smol

data FromExprState ann = FromExprState
  { fesModuleParts :: [IRModulePart],
    dataTypes :: Map Smol.TypeName (Smol.DataType ann),
    freshInt :: Int,
    vars :: Map IRIdentifier IRExpr
  }
