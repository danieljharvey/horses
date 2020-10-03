{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Scope where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

-- dependencies resolved into actual expressions
newtype Scope a = Scope {getScope :: Map Variable (Expr a Variable)}
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

instance Printer (Scope a) where
  prettyPrint (Scope s) = "{ " <> T.intercalate ", " (printItem <$> M.toList s) <> " }"
    where
      printItem (k, a) = prettyPrint k <> ": " <> prettyPrint a
