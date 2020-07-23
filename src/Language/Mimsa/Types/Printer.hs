{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Printer where

import Data.Text (Text)
import qualified Data.Text as T

class Printer a where
  prettyPrint :: a -> Text
  default prettyPrint :: (Show a) => a -> Text
  prettyPrint = T.pack . show

instance (Printer a, Printer b) => Printer (a, b) where
  prettyPrint (a, b) = prettyPrint a <> ": " <> prettyPrint b
