{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Types
  ( TCEnv (..),
    Typeclass (..),
    TypeclassHead (..),
    Instance (..),
    module Smol.Core.Typecheck.Types.TCError,
    module Smol.Core.Typecheck.Types.TCState,
    module Smol.Core.Typecheck.Types.TCWrite,
  )
where

import Control.Monad.Identity
import Data.Map.Strict (Map)
import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Core.Typecheck.Types.TCError
import Smol.Core.Typecheck.Types.TCState
import Smol.Core.Typecheck.Types.TCWrite
import Smol.Core.Types

-- | the typeclass described in it's most general form, ie
-- class Show a where show :: a -> String
data Typeclass ann = Typeclass
  { tcName :: String,
    tcArgs :: [Identifier],
    tcFuncName :: Identifier,
    tcFuncType :: Type Identity ann
  }
  deriving stock (Eq, Ord, Show)

data TypeclassHead ann
  = TypeclassHead String [Type Identity ann]
  deriving stock (Eq, Ord, Show)

instance Printer (TypeclassHead ann) where
  prettyDoc (TypeclassHead tcn tys) =
    PP.pretty tcn
      PP.<+> PP.concatWith
        (\a b -> a <> " " <> b)
        (prettyDoc <$> tys)

data Instance ann = Instance
  { inConstraints :: [TypeclassHead ann],
    inExpr :: Expr Identity ann
  }
  deriving stock (Eq, Ord, Show)

instance Printer (Instance ann) where
  prettyDoc (Instance [] expr) = prettyDoc expr
  prettyDoc (Instance constraints expr) =
    "(" <> PP.concatWith (\a b -> a <> ", " <> b) (prettyDoc <$> constraints) <> ") => " <> prettyDoc expr

data TCEnv ann = TCEnv
  { tceVars :: Map (ResolvedDep Identifier) (ResolvedType ann),
    tceGlobals :: Map Identifier (ResolvedType ann),
    tceDataTypes :: Map (ResolvedDep TypeName) (DataType ResolvedDep ann),
    tceClasses :: Map String (Typeclass ann),
    tceInstances :: Map (TypeclassHead ann) (Instance ann),
    tceConstraints :: [TypeclassHead ann]
  }
