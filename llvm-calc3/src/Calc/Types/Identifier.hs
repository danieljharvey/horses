{-# LANGUAGE DerivingStrategies #-}
  {-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Calc.Types.Identifier (Identifier(..),safeMkIdentifier) where

import Data.Hashable

import Data.Text (Text)
import Data.String
import qualified Data.Text as T
import qualified Prettyprinter as PP
import qualified Data.Char as Ch

newtype Identifier = Identifier Text
  deriving newtype (Eq,Ord,Show, Hashable)

instance IsString Identifier where
  fromString = Identifier . T.pack

instance PP.Pretty Identifier where
  pretty (Identifier ident) = PP.pretty ident

validIdentifier :: Text -> Bool
validIdentifier a =
  T.length a > 0
    && T.filter Ch.isAlphaNum a == a
    && not (Ch.isDigit (T.head a))
    && Ch.isLower (T.head a)

safeMkIdentifier :: Text -> Maybe Identifier
safeMkIdentifier a =
  if validIdentifier a
    then Just (Identifier a)
    else Nothing
