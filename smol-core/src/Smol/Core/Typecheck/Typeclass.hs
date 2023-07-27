{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Typeclass
  ( checkInstance,
    lookupInstanceAndCheck,
    inlineTypeclassFunctions,
    getTypeclassMethodNames,
    module Smol.Core.Typecheck.Typeclass.Helpers,
  )
where

import Control.Monad.Except
import Control.Monad.Identity
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Smol.Core.ExprUtils
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Typecheck.Elaborate (elaborate)
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Typeclass.Helpers
import Smol.Core.Typecheck.Types
import Smol.Core.Types

resolveExpr :: Expr Identity ann -> Expr ParseDep ann
resolveExpr = mapExprDep resolve
  where
    resolve (Identity a) = emptyParseDep a

lookupInstanceAndCheck ::
  (Ord ann, Monoid ann, Show ann, MonadError (TCError ann) m) =>
  TCEnv ann ->
  Constraint ann ->
  m (Identifier, Expr ResolvedDep (Type ResolvedDep ann))
lookupInstanceAndCheck env tch@(Constraint typeclassName _) = do
  tcInstance <- lookupTypeclassInstance env tch
  typeclass <- case M.lookup typeclassName (tceClasses env) of
    Just tc -> pure tc
    Nothing -> error "fuck"
  checkInstance env typeclass tch tcInstance

checkInstance ::
  (MonadError (TCError ann) m, Ord ann, Show ann, Monoid ann) =>
  TCEnv ann ->
  Typeclass ann ->
  Constraint ann ->
  Instance ann ->
  m (Identifier, Expr ResolvedDep (Type ResolvedDep ann))
checkInstance tcEnv (Typeclass _ args funcName ty) (Constraint _ tys) (Instance constraints expr) =
  do
    let subs =
          ( \(ident, tySub) ->
              Substitution (SubId $ Identity ident) tySub
          )
            <$> zip args tys

    let subbedType = substituteMany subs ty

    let -- we add the instance's constraints (so typechecker forgives a missing `Eq a` etc)
        typecheckEnv = tcEnv {tceConstraints = constraints}

        annotatedExpr = EAnn (getExprAnnotation expr) subbedType expr

        -- let's get all the method names from the Typeclasses
        -- mentioned in the instance constraints
        typeclassMethodNames =
          S.fromList $
            mapMaybe
              ( \(Constraint tcn _) -> case M.lookup tcn (tceClasses tcEnv) of
                  Just (Typeclass {tcFuncName}) -> Just tcFuncName
                  _ -> Nothing
              )
              constraints

    case resolveExprDeps (resolveExpr annotatedExpr) typeclassMethodNames of
      Left resolveErr -> error (show resolveErr)
      Right resolvedExpr -> do
        (typedExpr, _typeclassUses) <- elaborate typecheckEnv resolvedExpr

        pure (funcName, typedExpr)

-- let's get all the method names from the Typeclasses
-- mentioned in the instance constraints
getTypeclassMethodNames :: TCEnv ann -> S.Set Identifier
getTypeclassMethodNames tcEnv =
  S.fromList $
    tcFuncName <$> M.elems (tceClasses tcEnv)

-- | 10x typeclasses implementation - given an `expr` that calls typeclass
-- methods, we inline all the instances as Let bindings
-- `let equals_1 = \a -> \b -> a == b in equals_1 10 11`
inlineTypeclassFunctions ::
  (MonadError (TCError ann) m, Ord ann, Show ann, Monoid ann) =>
  TCEnv ann ->
  M.Map (ResolvedDep Identifier) (Constraint ann) ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
inlineTypeclassFunctions env tcs expr =
  foldrM tcHeadToLet expr (M.toList tcs)
  where
    tcHeadToLet (ident, typeclassHead) rest = do
      (_, instanceExpr) <- lookupInstanceAndCheck env typeclassHead
      let ty = getExprAnnotation rest
       in pure $ ELet ty ident instanceExpr rest
