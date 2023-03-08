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
  | TFunction ann [Type ann] (Type ann)
  deriving stock (Eq, Ord, Show, Functor)

instance PP.Pretty (Type ann) where
  pretty (TPrim _ prim) = PP.pretty prim
  pretty (TFunction _ args ret) =
    "(" <> prettyArgs <> ") -> " <> PP.pretty ret
      where
        prettyArgs = PP.concatWith (PP.surround PP.comma) (PP.pretty <$> args)
