{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.ResolverError where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Mimsa.Core
import Language.Mimsa.Types.Store.Bindings (Bindings (Bindings))
import Language.Mimsa.Types.Store.TypeBindings
  ( TypeBindings (TypeBindings),
  )

data ResolverError
  = MissingBinding Name Bindings
  | MissingType TyCon TypeBindings
  deriving stock (Eq, Ord, Show)

instance Printer ResolverError where
  prettyPrint (MissingBinding name (Bindings bindings')) = "A binding for " <> prettyPrint name <> " could not be found in " <> bindingsList
    where
      bindingsList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys bindings') <> " ]"
  prettyPrint (MissingType cName (TypeBindings bindings')) = "A binding for type " <> prettyPrint cName <> " could not be found in " <> bindingsList
    where
      bindingsList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys bindings') <> " ]"
