{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Types.Pattern
  ( Pattern (..),
    printSubPattern,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)
import qualified Prettyprinter as PP
import Smol.Core.Types.Constructor
import Smol.Core.Types.Identifier
import Smol.Core.Types.Prim
import Smol.Core.Types.Spread

data Pattern dep ann
  = PWildcard ann
  | PVar ann (dep Identifier)
  | PTuple ann (Pattern dep ann) (NE.NonEmpty (Pattern dep ann))
  | PArray ann [Pattern dep ann] (Spread dep ann)
  | PLiteral ann Prim
  | PConstructor ann (dep Constructor) [Pattern dep ann]
  deriving stock (Functor, Foldable, Generic, Traversable)

deriving stock instance
  ( Eq ann,
    Eq (dep Identifier),
    Eq (dep Constructor)
  ) =>
  Eq (Pattern dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep Identifier),
    Ord (dep Constructor)
  ) =>
  Ord (Pattern dep ann)

deriving stock instance
  ( Show ann,
    Show (dep Identifier),
    Show (dep Constructor)
  ) =>
  Show (Pattern dep ann)

deriving anyclass instance
  ( FromJSON ann,
    FromJSON (dep Identifier),
    FromJSON (dep Constructor)
  ) =>
  FromJSON (Pattern dep ann)

deriving anyclass instance
  ( ToJSON ann,
    ToJSON (dep Identifier),
    ToJSON (dep Constructor)
  ) =>
  ToJSON (Pattern dep ann)

_inParens :: (PP.Pretty a) => a -> PP.Doc style
_inParens = PP.parens . PP.pretty

-- print simple things with no brackets, and complex things inside brackets
printSubPattern ::
  ( PP.Pretty (dep Constructor),
    PP.Pretty (dep Identifier)
  ) =>
  Pattern dep ann ->
  PP.Doc style
printSubPattern pat = case pat of
  all'@PConstructor {} -> PP.pretty all' -- inParens all'
  a -> PP.pretty a

instance
  ( PP.Pretty (dep Constructor),
    PP.Pretty (dep Identifier)
  ) =>
  PP.Pretty (Pattern dep ann)
  where
  pretty (PWildcard _) = "_"
  pretty (PVar _ a) = PP.pretty a
  pretty (PLiteral _ lit) = PP.pretty lit
  pretty (PConstructor _ tyCon args) =
    let prettyArgs = case args of
          [] -> mempty
          _ -> foldr ((\a b -> " " <> a <> b) . printSubPattern) mempty args
     in PP.pretty tyCon <> prettyArgs
  pretty (PTuple _ a as) =
    "(" <> PP.hsep (PP.punctuate "," (PP.pretty <$> ([a] <> NE.toList as))) <> ")"
  pretty (PArray _ as spread) =
    "["
      <> PP.concatWith
        (\a b -> a <> ", " <> b)
        (PP.pretty <$> as)
      <> PP.pretty spread
      <> "]"

