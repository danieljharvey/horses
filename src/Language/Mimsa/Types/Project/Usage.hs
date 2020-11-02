{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Project.Usage where

import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.ExprHash

data Usage
  = Transient Name ExprHash
  | Direct Name ExprHash
  deriving (Eq, Ord, Show)

instance Printer Usage where
  prettyPrint (Transient name _) =
    "Transient dependency of "
      <> prettyPrint name
  prettyPrint (Direct name _) =
    "Direct dependency of "
      <> prettyPrint name
----------
