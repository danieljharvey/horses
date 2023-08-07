{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Smol.Core.Typecheck.Typeclass.Types
  (
    Typeclass (..),
    Constraint (..),
    Instance (..),
    module Smol.Core.Typecheck.Typeclass.Types.TypeclassName
  )
where

import Smol.Core.Typecheck.Typeclass.Types.TypeclassName
import Control.Monad.Identity
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Core.Types

-- | the typeclass described in it's most general form, ie
-- class Show a where show :: a -> String
data Typeclass ann = Typeclass
  { tcName :: TypeclassName,
    tcArgs :: [Identifier],
    tcFuncName :: Identifier,
    tcFuncType :: Type Identity ann
  }
  deriving stock (Eq, Ord, Show, Functor)

data Constraint ann
  = Constraint TypeclassName [Type Identity ann]
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  deriving stock (Eq, Ord, Show, Functor)

instance Printer (Instance ann) where
  prettyDoc (Instance [] expr) = prettyDoc expr
  prettyDoc (Instance constraints expr) =
    "(" <> PP.concatWith (\a b -> a <> ", " <> b) (prettyDoc <$> constraints) <> ") => " <> prettyDoc expr