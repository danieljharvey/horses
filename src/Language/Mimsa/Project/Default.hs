{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.Default
  ( defaultProject,
  )
where

import Language.Mimsa.Types

servers :: [ServerUrl]
servers = pure (ServerUrl "https://raw.githubusercontent.com/danieljharvey/mimsa-store/master/")

defaultProject :: Project ann
defaultProject =
  Project mempty mempty mempty servers
