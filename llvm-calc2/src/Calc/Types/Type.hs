{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Type (Type (..), TypePrim (..)) where

import qualified Prettyprinter as PP

data TypePrim = TBool | TInt
  deriving stock (Eq, Ord, Show)

instance PP.Pretty TypePrim where
  pretty TBool = "Boolean"
  pretty TInt = "Integer"

data Type ann
  = TPrim ann TypePrim
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Type ann) where
  pretty (TPrim _ prim) = PP.pretty prim
