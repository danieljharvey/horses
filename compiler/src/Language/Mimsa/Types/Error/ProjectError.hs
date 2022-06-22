{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.ProjectError (ProjectError (..)) where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Modules

-- | things that could happen
data ProjectError
  = CantUpgradeNoDependencies
  | CantUpgradeAlreadyUpToDate
  | CannotFindModuleByName ModuleName (Set ModuleName)
  deriving stock (Eq, Ord, Show)

instance Printer ProjectError where
  prettyPrint CantUpgradeAlreadyUpToDate =
    "Cannot upgrade, dependencies are already up to date"
  prettyPrint CantUpgradeNoDependencies =
    "Cannot upgrade, expression has no dependencies"
  prettyPrint (CannotFindModuleByName needle haystack) =
    "Could not find a module called " <> prettyPrint needle <> " in project. Found "
      <> T.intercalate ", " (prettyPrint <$> S.toList haystack)
      <> "."
