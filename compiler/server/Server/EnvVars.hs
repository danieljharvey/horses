{-# LANGUAGE OverloadedStrings #-}

module Server.EnvVars
  ( getMimsaEnv,
  )
where

import Control.Monad.Except
import Language.Mimsa.Types.MimsaConfig
import System.Envy

-- All fields will be converted to uppercase
getMimsaEnv :: ExceptT String IO MimsaConfig
getMimsaEnv =
  ExceptT $
    runEnv $
      gFromEnvCustom
        defOption
        (Just (MimsaConfig 8999 "./.mimsa" False Nothing))
