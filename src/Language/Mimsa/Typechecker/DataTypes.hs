{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.DataTypes where

import qualified Data.Map as M
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.Environment

defaultEnv :: Environment
defaultEnv = Environment mempty dts
  where
    dts =
      M.fromList [(mkConstruct "String", mempty), (mkConstruct "Int", mempty)]
