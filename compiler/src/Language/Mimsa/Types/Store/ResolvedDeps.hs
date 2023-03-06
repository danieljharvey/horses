{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.Store.ResolvedDeps where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Language.Mimsa.Core
import Language.Mimsa.Types.Store.ExprHash
import Language.Mimsa.Types.Store.StoreExpression
import Prettyprinter

newtype ResolvedDeps a = ResolvedDeps
  { getResolvedDeps ::
      Map
        (Maybe ModuleName, Name)
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
