{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Typeclass
  ( checkInstance,
    lookupInstanceAndCheck,
    inlineTypeclassFunctions,
    module Smol.Core.Typecheck.Typeclass.Helpers,
  )
where

import Control.Monad.Except
import Control.Monad.Identity
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as M
import Smol.Core.ExprUtils
import Smol.Core.Typecheck.Elaborate (elaborate)
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Typeclass.Helpers
import Smol.Core.Typecheck.Types
import Smol.Core.Types

resolveExpr :: Expr Identity ann -> Expr ResolvedDep ann
resolveExpr = mapExprDep resolve
  where
    resolve (Identity a) = emptyResolvedDep a

lookupInstanceAndCheck ::
  (Ord ann, Monoid ann, Show ann, MonadError (TCError ann) m) =>
  TCEnv ann ->
  TypeclassHead ann ->
  m (Identifier, Expr ResolvedDep (Type ResolvedDep ann))
lookupInstanceAndCheck env tch@(TypeclassHead typeclassName _) = do
  tcInstance <- lookupTypeclassHead env tch
  typeclass <- case M.lookup typeclassName (tceClasses env) of
    Just tc -> pure tc
    Nothing -> error "fuck"
  checkInstance typeclass tch tcInstance

checkInstance ::
  (MonadError (TCError ann) m, Ord ann, Show ann, Monoid ann) =>
  Typeclass ann ->
  TypeclassHead ann ->
  Instance ann ->
  m (Identifier, Expr ResolvedDep (Type ResolvedDep ann))
checkInstance (Typeclass _ args funcName ty) (TypeclassHead _ tys) (Instance _ expr) =
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

    (typedExpr, _typeclassUses) <- elaborate tcEnv (resolveExpr annotatedExpr)

    pure (funcName, typedExpr)

-- | 10x typeclasses implementation - we inline all the instances as Let
-- bindings
-- `let equals_1 = \a -> \b -> a == b in equals_1 10 11`
inlineTypeclassFunctions ::
  (MonadError (TCError ann) m, Ord ann, Show ann, Monoid ann) =>
  TCEnv ann ->
  M.Map (ResolvedDep Identifier) (TypeclassHead ann) ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
inlineTypeclassFunctions env tcs expr =
  foldrM tcHeadToLet expr (M.toList tcs)
  where
    tcHeadToLet (ident, typeclassHead) rest = do
      (_, instanceExpr) <- lookupInstanceAndCheck env typeclassHead
      let ty = getExprAnnotation rest
       in pure $ ELet ty ident instanceExpr rest
