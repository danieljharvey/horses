{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Typecheck.Typeclass (checkInstance) where

import Control.Monad.Except
import Control.Monad.Identity
import Smol.Core.ExprUtils (mapExprDep)
import Smol.Core.Typecheck.Elaborate (elaborate, getExprAnnotation)
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Types
import Smol.Core.Types.Expr
import Smol.Core.Types.Identifier
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.Type

resolveExpr :: Expr Identity ann -> Expr ResolvedDep ann
resolveExpr = mapExprDep resolve
  where
    resolve (Identity a) = emptyResolvedDep a

checkInstance ::
  (MonadError (TCError ann) m, Ord ann, Show ann) =>
  Typeclass ann ->
  TypeclassHead ann ->
  Instance ann ->
  m (Identifier, Expr ResolvedDep (Type ResolvedDep ann))
checkInstance (Typeclass _ args funcName ty) (TypeclassHead _ tys) (Instance expr) =
  do
    let subs =
          ( \(ident, tySub) ->
              Substitution (SubId $ Identity ident) tySub
          )
            <$> zip args tys

    let subbedType = substituteMany subs ty

    let tcEnv =
          TCEnv
            { tceVars = mempty,
              tceGlobals = mempty,
              tceDataTypes = mempty,
              tceClasses = mempty,
              tceInstances = mempty
            }

    let annotatedExpr = EAnn (getExprAnnotation expr) subbedType expr

    typedExpr <- elaborate tcEnv (resolveExpr annotatedExpr)

    pure (funcName, typedExpr)
