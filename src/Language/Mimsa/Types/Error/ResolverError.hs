{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.ResolverError where

import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Printer (Printer (prettyPrint))
import Language.Mimsa.Types.Identifiers (Name, TyCon)
import Language.Mimsa.Types.Store.Bindings (Bindings (Bindings))
import Language.Mimsa.Types.Store.TypeBindings
  ( TypeBindings (TypeBindings),
  )

data ResolverError
  = MissingBinding Name Bindings
  | MissingType TyCon TypeBindings
  deriving (Eq, Ord, Show)

instance Printer ResolverError where
  prettyPrint (MissingBinding name (Bindings bindings')) = "A binding for " <> prettyPrint name <> " could not be found in " <> bindingsList
    where
      bindingsList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys bindings') <> " ]"
  prettyPrint (MissingType cName (TypeBindings bindings')) = "A binding for type " <> prettyPrint cName <> " could not be found in " <> bindingsList
    where
      bindingsList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys bindings') <> " ]"
