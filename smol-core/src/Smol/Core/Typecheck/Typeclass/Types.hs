{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Typeclass.Types
  ( Typeclass (..),
    Constraint (..),
    Instance (..),
    module Smol.Core.Typecheck.Typeclass.Types.TypeclassName,
  )
where

import Control.Monad.Identity
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import GHC.Generics
import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Core.Typecheck.Typeclass.Types.TypeclassName
import Smol.Core.Types

-- | the typeclass described in it's most general form, ie
-- class Show a where show :: a -> String
data Typeclass ann = Typeclass
  { tcName :: TypeclassName,
    tcArgs :: [Identifier],
    tcFuncName :: Identifier,
    tcFuncType :: Type Identity ann
  }
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Constraint ann = Constraint {conTypeclass :: TypeclassName, conType :: [Type Identity ann]}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic)
  deriving anyclass (ToJSON, ToJSONKey, FromJSON, FromJSONKey)

instance Printer (Constraint ann) where
  prettyDoc (Constraint tcn tys) =
    prettyDoc tcn
      PP.<+> PP.concatWith
        (\a b -> a <> " " <> b)
        (prettyDoc <$> tys)

data Instance ann = Instance
  { inConstraints :: [Constraint ann],
    inExpr :: Expr Identity ann
  }
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Printer (Instance ann) where
  prettyDoc (Instance [] expr) = prettyDoc expr
  prettyDoc (Instance constraints expr) =
    "(" <> PP.concatWith (\a b -> a <> ", " <> b) (prettyDoc <$> constraints) <> ") => " <> prettyDoc expr
