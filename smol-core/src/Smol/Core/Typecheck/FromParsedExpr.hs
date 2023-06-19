-- don't use this!
module Smol.Core.Typecheck.FromParsedExpr (fromParsedExpr, fromParsedType) where

import Smol.Core.ExprUtils
import Smol.Core.Types.Expr
import Smol.Core.Types.ParseDep
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.Type

resolve :: ParseDep a -> ResolvedDep a
resolve (ParseDep a _) = emptyResolvedDep a

-- | `ParsedExpr` has module names
-- | `ResolvedExpr` has module hashes and unique ids
-- this is like NumberVars from main `mimsa`, but for now we'll bodge it
-- to get things typechecking
fromParsedExpr :: ParsedExpr ann -> ResolvedExpr ann
fromParsedExpr = mapExprDep resolve

fromParsedType :: Type ParseDep ann -> Type ResolvedDep ann
fromParsedType = mapTypeDep resolve
