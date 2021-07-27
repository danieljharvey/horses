{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.Scheme where

import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker.MonoType

data Scheme = Scheme [TypeIdentifier] MonoType
  deriving stock (Eq, Ord, Show)

instance Printer Scheme where
  prettyPrint (Scheme vars mt) = varText <> prettyPrint mt
    where
      varText = case vars of
        [] -> ""
        a -> "[" <> T.intercalate ", " (prettyPrint <$> a) <> "] "
