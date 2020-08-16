{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.DataTypes where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.Environment
import Language.Mimsa.Types.MonoType

defaultEnv :: Environment
defaultEnv = Environment mempty dts
  where
    dts =
      M.fromList [(mkConstruct "String", mempty), (mkConstruct "Int", mempty)]

builtInTypes :: Map Construct MonoType
builtInTypes = M.fromList [(mkConstruct "String", MTString), (mkConstruct "Int", MTInt)]
