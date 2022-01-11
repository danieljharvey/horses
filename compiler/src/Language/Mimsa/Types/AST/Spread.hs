{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.Spread
  ( Spread (..),
  )
where

import qualified Data.Aeson as JSON
import GHC.Generics
import Language.Mimsa.Printer

data Spread var ann
  = NoSpread
  | SpreadWildcard
      { sprAnn :: ann
      }
  | SpreadValue
      { sprAnn :: ann,
        sprVar :: var
      }
  deriving stock
    ( Show,
      Eq,
      Ord,
      Functor,
      Foldable,
      Generic
    )
  deriving anyclass
    ( JSON.FromJSON,
      JSON.ToJSON
    )

instance (Printer var, Show var) => Printer (Spread var ann) where
  prettyDoc NoSpread = ""
  prettyDoc (SpreadWildcard _) = ", ..."
  prettyDoc (SpreadValue _ a) = ", ..." <> prettyDoc a
