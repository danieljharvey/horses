module Smol.Backend.IR.FromResolvedExpr (fromResolvedExpr, fromResolvedType) where

import Control.Monad.Identity
import Smol.Core.ExprUtils
import Smol.Core.Types.Expr
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.Type

-- for now, throw extra info away
resolve :: ResolvedDep a -> Identity a
resolve (LocalDefinition a) = pure a
resolve (ModuleDefinition a _) = pure a
resolve (UniqueDefinition a _) = pure a

-- | We have a ResolvedDep with lots of info, but when it comes to compiling
-- we don't want to leak all that shit. `IdentityExpr` is no doubt the wrong
-- choice but fuck it
fromResolvedExpr :: ResolvedExpr ann -> IdentityExpr ann
fromResolvedExpr = mapExprDep resolve

fromResolvedType :: Type ResolvedDep ann -> Type Identity ann
fromResolvedType = mapTypeDep resolve
