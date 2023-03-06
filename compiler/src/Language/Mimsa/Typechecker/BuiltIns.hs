{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.BuiltIns
  ( builtInTypes,
    lookupBuiltIn,
  )
where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Language.Mimsa.Core

builtInTypes :: Map TypeName MonoType
builtInTypes =
  M.fromList
    [ ("String", MTPrim mempty MTString),
      ("Int", MTPrim mempty MTInt),
      ("Boolean", MTPrim mempty MTBool)
    ]

lookupBuiltIn :: TypeName -> Maybe MonoType
lookupBuiltIn name = M.lookup name builtInTypes
