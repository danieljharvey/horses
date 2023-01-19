module Smol.Core.Typecheck.FromParsedExpr (fromParsedExpr) where

import Smol.Core.ExprUtils
import Smol.Core.Types.Expr
import Smol.Core.Types.ParseDep
import Smol.Core.Types.ResolvedDep

resolve :: ParseDep a -> ResolvedDep a
resolve (ParseDep a _) = emptyResolvedDep a

-- | `ParsedExpr` has module names
-- | `ResolvedExpr` has module hashes and unique ids
-- this is like NumberVars from main `mimsa`, but for now we'll bodge it
-- to get things typechecking
fromParsedExpr :: ParsedExpr ann -> ResolvedExpr ann
fromParsedExpr = mapExprDep resolve
