{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Error.ProjectError (ProjectError (..)) where

import Language.Mimsa.Printer

-- | things that could happen
data ProjectError
  = CantUpgradeNoDependencies
  | CantUpgradeAlreadyUpToDate
  deriving stock (Eq, Ord, Show)

instance Printer ProjectError where
  prettyPrint CantUpgradeAlreadyUpToDate =
    "Cannot upgrade, dependencies are already up to date"
  prettyPrint CantUpgradeNoDependencies =
    "Cannot upgrade, expression has no dependencies"
