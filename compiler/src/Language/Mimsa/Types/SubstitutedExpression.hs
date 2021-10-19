{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.SubstitutedExpression where

import Data.Map (Map)
import Data.Set (Set)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Scope
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Swaps

data SubstitutedExpression ann = SubstitutedExpression
  { seSwaps :: Swaps,
    seExpr :: Expr Variable ann,
    seScope :: Scope ann,
    seDeps :: Map Name (StoreExpression ann),
    seTypeDeps :: Set (StoreExpression ann)
  }
  deriving stock (Eq, Ord, Show)
