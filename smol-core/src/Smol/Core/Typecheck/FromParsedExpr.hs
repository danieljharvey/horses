module Smol.Core.Typecheck.FromParsedExpr (fromParsedExpr) where

import Smol.Core.Types.Expr

-- | `ParsedExpr` has module names
-- | `ResolvedExpr` has module hashes and unique ids
-- this is like NumberVars from main `mimsa`, but for now we'll bodge it
-- to get things typechecking

fromParsedExpr :: ParsedExpr ann -> ResolvedExpr ann
fromParsedExpr _ = undefined
