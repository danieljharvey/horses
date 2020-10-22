{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Annotation where

import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer

data Annotation
  = None
  | Location Int Int
  deriving (Eq, Ord, Show, Generic)

instance Semigroup Annotation where
  Location a b <> Location a' b' = Location (min a a') (max b b')
  Location a b <> None = Location a b
  None <> a = a

instance Monoid Annotation where
  mempty = None

instance Printer Annotation where
  prettyDoc None = "-"
  prettyDoc (Location a b) = pretty a <> " - " <> pretty b
