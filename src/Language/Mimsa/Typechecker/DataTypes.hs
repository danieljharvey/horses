{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.DataTypes
  ( defaultEnv,
    builtInTypes,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Environment
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.MonoType

defaultEnv :: Environment
defaultEnv = Environment mempty dts
  where
    makeDT (name, _) = M.singleton name (DataType name mempty mempty)
    dts = mconcat $ makeDT <$> M.toList builtInTypes

builtInTypes :: Map Construct MonoType
builtInTypes =
  M.fromList
    [ (mkConstruct "String", MTString),
      (mkConstruct "Int", MTInt),
      (mkConstruct "Boolean", MTBool),
      (mkConstruct "Unit", MTUnit)
    ]
