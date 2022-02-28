{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Types.AST.StringPart
  ( StringPart (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Bifunctor.TH
import GHC.Generics
import Language.Mimsa.Printer

data StringPart var ann
  = StrWildcard
      { stpAnn :: ann
      }
  | StrValue
      { stpAnn :: ann,
        stpVar :: var
      }
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
  deriving anyclass (JSON.FromJSON, JSON.ToJSON)

$(deriveBifunctor ''StringPart)

instance (Printer var) => Printer (StringPart var ann) where
  prettyDoc (StrWildcard _) = "_"
  prettyDoc (StrValue _ a) = prettyDoc a
