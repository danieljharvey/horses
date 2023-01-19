module Smol.Core.Typecheck.FromParsedExpr (fromParsedExpr) where

import Data.Bifunctor (bimap)
import Smol.Core.Types.Expr
import Smol.Core.Types.ParseDep
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.Pattern

resolve :: ParseDep a -> ResolvedDep a
resolve (ParseDep a _) = emptyResolvedDep a

-- | `ParsedExpr` has module names
-- | `ResolvedExpr` has module hashes and unique ids
-- this is like NumberVars from main `mimsa`, but for now we'll bodge it
-- to get things typechecking
fromParsedExpr :: ParsedExpr ann -> ResolvedExpr ann
fromParsedExpr = go
  where
    go :: Expr ParseDep ann -> Expr ResolvedDep ann
    go  (EInfix ann op a b) = EInfix ann op (go a) (go b)
    go  (EAnn ann mt expr) = EAnn ann mt (go expr)
    go  (EPrim ann a) = EPrim ann a
    go  (EVar ann a ) =
                    EVar ann (resolve a)
    go  (EConstructor ann a ) =
                  EConstructor ann (resolve a)
    go  (ELet ann ident expr rest) =
        ELet ann (resolve ident) (go expr) (go rest)
    go  (ELambda ann ident body) = ELambda ann (resolve ident) (go body)
    go  (EApp ann fn arg) = EApp ann (go fn) (go arg)
    go  (EIf ann predExp thenExp elseExp) =
      EIf ann (go predExp) (go thenExp) (go elseExp)
    go  (ETuple ann a as) = ETuple ann (go a) (go <$> as)
    go  (EGlobal ann ident) = EGlobal ann ident
    go  (EGlobalLet ann ident expr rest) =
      EGlobalLet ann ident (go expr) (go rest)
    go  (ERecord ann as) = ERecord ann (go <$> as)
    go  (ERecordAccess ann expr ident) =
      ERecordAccess ann (go expr) ident
    go  (EPatternMatch ann patExpr pats) =
      EPatternMatch ann (go patExpr) (bimap fromParsedPattern go <$> pats)

fromParsedPattern :: Pattern ParseDep ann -> Pattern ResolvedDep ann
fromParsedPattern = go
  where
    go  (PLiteral ann l) = PLiteral ann l
    go  (PWildcard a) = PWildcard a
    go  (PVar ann a) = PVar ann (resolve a)
    go  (PTuple ann a as) = PTuple ann (go a) (go <$> as)
    go  (PConstructor ann constructor as) =
      PConstructor ann (resolve constructor) (go <$> as)


