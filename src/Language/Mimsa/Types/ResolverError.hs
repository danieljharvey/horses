{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.ResolverError where

import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Types.Bindings
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Printer

data ResolverError = MissingBinding Name Bindings
  deriving (Eq, Ord, Show)

instance Printer ResolverError where
  prettyPrint (MissingBinding name (Bindings bindings')) = "A binding for " <> prettyPrint name <> " could not be found in " <> bindingsList
    where
      bindingsList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys bindings') <> " ]"
