{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Smol.Backend.IR.FromResolvedExpr
  ( fromResolvedModule,
    fromResolvedExpr,
    fromResolvedType,
  )
where

import Control.Monad.Identity
import Smol.Core.ExprUtils
import Smol.Core.Types.DataType
import Smol.Core.Types.Expr
import Smol.Core.Types.Module
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.Type

-- for now, throw extra info away
resolve :: ResolvedDep a -> Identity a
resolve (LocalDefinition a) = pure a
resolve (UniqueDefinition a _) = pure a

-- | We have a ResolvedDep with lots of info, but when it comes to compiling
-- we don't want to leak all that shit. `IdentityExpr` is no doubt the wrong
-- choice but fuck it
fromResolvedModule :: Module ResolvedDep ann -> Module Identity ann
fromResolvedModule (Module {..}) =
  Module
    { moExpressionExports,
      moExpressionImports,
      moDataTypeExports,
      moDataTypeImports,
      moNamedImports,
      moExpressions = fromResolvedExpr <$> moExpressions,
      moDataTypes = fromResolvedDataType <$> moDataTypes
    }

fromResolvedDataType :: DataType ResolvedDep ann -> DataType Identity ann
fromResolvedDataType = mapDataTypeDep resolve

fromResolvedExpr :: Expr ResolvedDep ann -> Expr Identity ann
fromResolvedExpr = mapExprDep resolve

fromResolvedType :: Type ResolvedDep ann -> Type Identity ann
fromResolvedType = mapTypeDep resolve
