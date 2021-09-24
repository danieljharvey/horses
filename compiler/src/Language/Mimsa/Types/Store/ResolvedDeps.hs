{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Store.ResolvedDeps where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.ExprHash
import Language.Mimsa.Types.Store.StoreExpression
import Prettyprinter

newtype ResolvedDeps a = ResolvedDeps
  { getResolvedDeps ::
      Map
        Name
        (ExprHash, StoreExpression a)
  }
  deriving newtype (Semigroup, Monoid)

hasNoDeps :: ResolvedDeps a -> Bool
hasNoDeps (ResolvedDeps m) = M.size m == 0

instance Printer (ResolvedDeps a) where
  prettyDoc (ResolvedDeps deps) =
    encloseSep
      lbrace
      rbrace
      comma
      (prettyDoc <$> M.keys deps)
