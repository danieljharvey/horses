{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.ResolverError where

import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Bindings
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.TypeBindings

data ResolverError
  = MissingBinding Name Bindings
  | MissingType Construct TypeBindings
  deriving (Eq, Ord, Show)

instance Printer ResolverError where
  prettyPrint (MissingBinding name (Bindings bindings')) = "A binding for " <> prettyPrint name <> " could not be found in " <> bindingsList
    where
      bindingsList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys bindings') <> " ]"
  prettyPrint (MissingType cName (TypeBindings bindings')) = "A binding for type " <> prettyPrint cName <> " could not be found in " <> bindingsList
    where
      bindingsList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys bindings') <> " ]"
