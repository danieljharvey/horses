module Smol.Core.IR.FromResolvedExpr (fromResolvedExpr) where

import Smol.Core.Types.Expr

-- | We have a ResolvedDep with lots of info, but when it comes to compiling
-- we don't want to leak all that shit. `IdentityExpr` is no doubt the wrong
-- choice but fuck it

fromResolvedExpr :: ResolvedExpr ann -> IdentityExpr ann
fromResolvedExpr _ = undefined
