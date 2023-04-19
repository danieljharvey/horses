{-# LANGUAGE DerivingStrategies #-}
  {-# LANGUAGE OverloadedStrings #-}
module Smol.Core.Modules.ResolveDeps
  (
    resolveExprDeps
  )
where

import Debug.Trace
import Smol.Core.Types.Module.Module
import Smol.Core
import Smol.Core.Modules.Dependencies
import Smol.Core.Types.Module.DefIdentifier
import Smol.Core.Modules.Uses
import qualified Data.Map.Strict as M

-- this is possibly only useful for testing
resolveExprDeps :: (Eq ann, Show ann) => Expr ParseDep ann -> Expr ResolvedDep ann
resolveExprDeps expr =
  -- create module with expr in
  let myMod = Module { moExpressions = M.singleton (DIName "main") expr,
      moExpressionExports = mempty,
    moExpressionImports = mempty,
    moDataTypes = mempty,
    moDataTypeExports = mempty,
    moDataTypeImports = mempty,
    moNamedImports = mempty
  }

  -- run get deps bullshit
   in case getDependencies extractUses myMod of
    Right map' -> -- extract result
          case M.lookup (DIName "main") map' of
            Just yes ->
              const undefined (traceShowId yes)
            Nothing -> error "did not find my boy"
    Left e -> error (show e)


