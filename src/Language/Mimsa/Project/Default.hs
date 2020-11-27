module Language.Mimsa.Project.Default
  ( defaultProject,
  )
where

import Language.Mimsa.Types.Project

defaultProject :: Project ann
defaultProject =
  Project mempty mempty mempty
