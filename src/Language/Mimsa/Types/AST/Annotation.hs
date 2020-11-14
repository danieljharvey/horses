{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Annotation where

import qualified Data.Aeson as JSON
import Data.OpenApi
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer

data Annotation
  = None ()
  | Location Int Int
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, JSON.FromJSON, ToSchema)

instance Semigroup Annotation where
  Location a b <> Location a' b' = Location (min a a') (max b b')
  Location a b <> None _ = Location a b
  None _ <> a = a

instance Monoid Annotation where
  mempty = None ()

instance Printer Annotation where
  prettyDoc (None _) = "-"
  prettyDoc (Location a b) = pretty a <> " - " <> pretty b
