{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Annotation where

import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer

data Annotation
  = None
  | Location Int
  deriving (Eq, Ord, Show, Generic)

instance Semigroup Annotation where
  Location a <> Location a' = Location (min a a')
  Location a <> None = Location a
  None <> a = a

instance Monoid Annotation where
  mempty = None

instance Printer Annotation where
  prettyDoc None = "-"
  prettyDoc (Location a) = pretty a
