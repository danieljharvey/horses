{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Scope where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

-- dependencies resolved into actual expressions
newtype Scope ann
  = Scope
      { getScope :: Map Variable (Expr Variable ann)
      }
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)
  deriving stock (Functor, Generic)
  deriving anyclass (JSON.ToJSON)

instance (Show ann, Printer ann) => Printer (Scope ann) where
  prettyPrint (Scope s) = "{ " <> T.intercalate ", " (printItem <$> M.toList s) <> " }"
    where
      printItem (k, a) = prettyPrint k <> ": " <> prettyPrint a
