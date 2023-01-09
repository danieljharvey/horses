{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Types.Identifier
  ( Identifier (..),
    safeMkIdentifier,
    mkIdentifier,
  )
where

import qualified Data.Char as Ch
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Prettyprinter as PP
import Smol.Core.Printer

newtype Identifier = Identifier Text
  deriving newtype (Eq, Ord, Show)

instance Printer Identifier where
  prettyDoc (Identifier i) = PP.pretty i

instance IsString Identifier where
  fromString = Identifier . T.pack

validIdentifier :: Text -> Bool
validIdentifier a =
  T.length a > 0
    && T.filter Ch.isAlphaNum a == a
    && not (Ch.isDigit (T.head a))
    && Ch.isLower (T.head a)

mkIdentifier :: Text -> Identifier
mkIdentifier a =
  if validIdentifier a
    then Identifier a
    else error $ T.unpack $ "Identifier validation fail for '" <> a <> "'"

safeMkIdentifier :: Text -> Maybe Identifier
safeMkIdentifier a =
  if validIdentifier a
    then Just (Identifier a)
    else Nothing
