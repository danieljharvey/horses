{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Types.Scheme where

import qualified Data.Text as T
import Language.Mimsa.Core

data Scheme = Scheme [TypeIdentifier] MonoType
  deriving stock (Eq, Ord, Show)

instance Printer Scheme where
  prettyPrint (Scheme vars mt) = varText <> prettyPrint mt
    where
      varText = case vars of
        [] -> ""
        a -> "[" <> T.intercalate ", " (prettyPrint <$> a) <> "] "
