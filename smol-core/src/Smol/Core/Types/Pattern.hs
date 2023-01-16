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
import Smol.Core.Printer
import Smol.Core.Types.Constructor
import Smol.Core.Types.Identifier
import Smol.Core.Types.ParseDep
import Smol.Core.Types.Prim

data Pattern dep ann
  = PWildcard ann
  | PVar ann (dep Identifier)
  | PTuple ann (Pattern dep ann) (NE.NonEmpty (Pattern dep ann))
  | PLiteral ann Prim
  | PConstructor ann Constructor [Pattern dep ann]
  deriving stock (Functor, Foldable, Generic, Traversable)

deriving stock instance
  ( Eq ann,
    Eq (dep Identifier)
  ) =>
  Eq (Pattern dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep Identifier)
  ) =>
  Ord (Pattern dep ann)

deriving stock instance
  ( Show ann,
    Show (dep Identifier)
  ) =>
  Show (Pattern dep ann)

deriving anyclass instance
  ( FromJSON ann,
    FromJSON (dep Identifier)
  ) =>
  FromJSON (Pattern dep ann)

deriving anyclass instance
  ( ToJSON ann,
    ToJSON (dep Identifier)
  ) =>
  ToJSON (Pattern dep ann)

inParens :: (Printer a) => a -> PP.Doc style
inParens = PP.parens . prettyDoc

-- print simple things with no brackets, and complex things inside brackets
printSubPattern :: Pattern ParseDep ann -> PP.Doc style
printSubPattern pat = case pat of
  all'@PConstructor {} -> inParens all'
  a -> prettyDoc a

instance Printer (Pattern ParseDep ann) where
  prettyDoc (PWildcard _) = "_"
  prettyDoc (PVar _ a) = prettyDoc a
  prettyDoc (PLiteral _ lit) = prettyDoc lit
  prettyDoc (PConstructor _ tyCon args) =
    let prettyArgs = case args of
          [] -> mempty
          _ -> foldr (\a b -> " " <> a <> b) mempty (printSubPattern <$> args)
     in prettyDoc tyCon <> prettyArgs
  prettyDoc (PTuple _ a as) =
    "(" <> PP.hsep (PP.punctuate ", " (prettyDoc <$> ([a] <> NE.toList as))) <> ")"

{-
  prettyDoc (PArray _ as spread) =
    "[" <> concatWith (\a b -> a <> ", " <> b) (prettyDoc <$> as) <> prettyDoc spread <> "]"
-}
{-
  prettyDoc (PRecord _ map') =
    let items = M.toList map'
        printRow i (name, val) =
          let item = case val of
                (PVar _ vName) | vName == name -> prettyDoc name
                _ ->
                  prettyDoc name
                    <> ":"
                    <+> printSubPattern val
           in item <> if i < length items then "," else ""
     in case items of
          [] -> "{}"
          rows ->
            let prettyRows = mapWithIndex printRow rows
             in group
                  ( "{"
                      <+> align
                        ( vsep
                            prettyRows
                        )
                      <+> "}"
                  )

-}
{-
  prettyDoc (PString _ a as) =
    prettyDoc a <> " ++ " <> prettyDoc as
-}
