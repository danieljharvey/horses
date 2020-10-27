{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.DataTypes
  ( defaultEnv,
    builtInTypes,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Types.AST (DataType (DataType))
import Language.Mimsa.Types.Environment (Environment (Environment))
import Language.Mimsa.Types.Identifiers (TyCon, mkTyCon)
import Language.Mimsa.Types.MonoType

defaultEnv :: Environment
defaultEnv = Environment mempty dts
  where
    makeDT (name, _) = M.singleton name (DataType name mempty mempty)
    dts = mconcat $ makeDT <$> M.toList builtInTypes

builtInTypes :: Map TyCon MonoType
builtInTypes =
  M.fromList
    [ (mkTyCon "String", MTPrim mempty MTString),
      (mkTyCon "Int", MTPrim mempty MTInt),
      (mkTyCon "Boolean", MTPrim mempty MTBool),
      (mkTyCon "Unit", MTPrim mempty MTUnit)
    ]
