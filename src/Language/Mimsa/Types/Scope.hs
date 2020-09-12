{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Scope where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Expr
import Language.Mimsa.Types.Identifiers

-- dependencies resolved into actual expressions
newtype Scope = Scope {getScope :: Map Variable (Expr Variable)}
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

instance Printer Scope where
  prettyPrint (Scope s) = "{ " <> T.intercalate ", " (printItem <$> M.toList s) <> " }"
    where
      printItem (k, a) = prettyPrint k <> ": " <> prettyPrint a
