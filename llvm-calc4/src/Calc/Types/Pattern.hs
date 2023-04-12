{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Calc.Types.Pattern (Pattern (..)) where

import Calc.Types.Identifier
import Calc.Types.Prim
import qualified Data.List.NonEmpty as NE
import GHC.Generics
import qualified Prettyprinter as PP

data Pattern ann
  = PWildcard ann
  | PVar ann Identifier
  | PTuple ann (Pattern ann) (NE.NonEmpty (Pattern ann))
  | PLiteral ann Prim
  deriving stock
    ( Eq,
      Ord,
      Show,
      Functor,
      Foldable,
      Generic,
      Traversable
    )

instance PP.Pretty (Pattern ann) where
  pretty (PWildcard _) = "_"
  pretty (PVar _ a) = PP.pretty a
  pretty (PLiteral _ lit) = PP.pretty lit
  pretty (PTuple _ a as) =
    "(" <> PP.hsep (PP.punctuate ", " (PP.pretty <$> ([a] <> NE.toList as))) <> ")"
