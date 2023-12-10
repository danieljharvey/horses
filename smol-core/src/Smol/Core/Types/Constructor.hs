{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Types.Constructor
  ( Constructor (..),
    safeMkConstructor,
    mkConstructor,
  )
where

import qualified Data.Aeson as JSON
import qualified Data.Char as Ch
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Prettyprinter as PP

newtype Constructor = Constructor Text
  deriving newtype
    ( Eq,
      Ord,
      Show,
      JSON.ToJSON,
      JSON.ToJSONKey,
      JSON.FromJSON,
      JSON.FromJSONKey,
      Semigroup
    )

instance PP.Pretty Constructor where
  pretty (Constructor c) = PP.pretty c

instance IsString Constructor where
  fromString = Constructor . T.pack

validConstructor :: Text -> Bool
validConstructor a =
  T.length a > 0
    && T.filter Ch.isAlphaNum a == a
    && not (Ch.isDigit (T.head a))
    && Ch.isUpper (T.head a)

mkConstructor :: Text -> Constructor
mkConstructor a =
  if validConstructor a
    then Constructor a
    else error $ T.unpack $ "Constructor validation fail for '" <> a <> "'"

safeMkConstructor :: Text -> Maybe Constructor
safeMkConstructor a =
  if validConstructor a
    then Just (Constructor a)
    else Nothing
