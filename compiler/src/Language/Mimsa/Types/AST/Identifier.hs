{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Types.AST.Identifier (Identifier (..)) where

import qualified Data.Aeson as JSON
import Data.Bifunctor.TH
import GHC.Generics
import Language.Mimsa.Printer

data Identifier var ann = Identifier
  { idAnn :: ann,
    idVar :: var
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

$(deriveBifunctor ''Identifier)

instance (Printer var) => Printer (Identifier var ann) where
  prettyDoc (Identifier _ var) = prettyDoc var
