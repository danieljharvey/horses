{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.ResolvedDeps where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Printer
import Language.Mimsa.Types.StoreExpression

newtype ResolvedDeps = ResolvedDeps {getResolvedDeps :: Map Name StoreExpression}

instance Printer ResolvedDeps where
  prettyPrint (ResolvedDeps deps) =
    "{"
      <> T.intercalate ", " (prettyPrint <$> M.keys deps)
      <> "}"

hasNoDeps :: ResolvedDeps -> Bool
hasNoDeps (ResolvedDeps m) = M.size m == 0
