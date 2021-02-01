{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.DataTypes
  ( defaultEnv,
    builtInTypes,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Types.AST (DataType (DataType))
import Language.Mimsa.Types.Identifiers (TyCon)
import Language.Mimsa.Types.Typechecker

defaultEnv :: Substitutions -> Environment
defaultEnv (Substitutions subst) = Environment schemes dts mempty
  where
    schemes = Scheme mempty <$> subst
    makeDT (name, _) = M.singleton name (DataType name mempty mempty)
    dts = mconcat $ makeDT <$> M.toList builtInTypes

builtInTypes :: Map TyCon MonoType
builtInTypes =
  M.fromList
    [ ("String", MTPrim mempty MTString),
      ("Int", MTPrim mempty MTInt),
      ("Boolean", MTPrim mempty MTBool),
      ("Unit", MTPrim mempty MTUnit)
    ]
