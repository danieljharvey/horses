{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Typeclass
  ( checkInstance,
    dedupeConstraints,
    lookupInstanceAndCheck,
    inlineTypeclassFunctions,
    getTypeclassMethodNames,
    module Smol.Core.Typecheck.Typeclass.Helpers,
  )
where

import Debug.Trace
import Control.Monad.Except
import Control.Monad.Identity
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Tuple (swap)
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

-- just because we use a method twice doesn't mean we want to pass it in twice
-- returns a new ordered set of constraints with fresh names,
-- and a list of substitutions to change in the expression to make everything
-- work
dedupeConstraints ::
  (Ord ann) =>
  M.Map (ResolvedDep Identifier) (Constraint ann) ->
  ([(ResolvedDep Identifier, Constraint ann)], M.Map (ResolvedDep Identifier) (ResolvedDep Identifier))
dedupeConstraints dupes =
  let initial = (mempty, mempty, 0)
      deduped =
        foldr
          ( \(ident, constraint) (found, swaps, count) ->
              case M.lookup constraint found of
                Just foundIdent ->
                  ( found,
                    swaps <> M.singleton ident foundIdent,
                    count
                  )
                Nothing ->
                  let newCount = count + 1
                      newIdent = TypeclassCall "newname" newCount
                   in ( found <> M.singleton constraint newIdent,
                        swaps <> M.singleton ident newIdent,
                        newCount
                      )
          )
          initial
          (M.toList dupes)
      (finalFound, finalSwaps, _) = deduped
   in (swap <$> M.toList finalFound, finalSwaps)

-- | swap var names of typeclass calls for their new deduped ones
swapExprVarnames :: M.Map (ResolvedDep Identifier) (ResolvedDep Identifier) -> Expr ResolvedDep ann -> Expr ResolvedDep ann
swapExprVarnames swappies expr =
  go expr
  where
    go (EVar ann ident) = case M.lookup (traceShowId ident) (traceShowId swappies) of
      Just newIdent -> EVar ann newIdent
      Nothing -> EVar ann ident
    go other = mapExpr go other

getTypeForDictionary ::
  ( MonadError (TCError ann) m,
    Ord ann,
    Show ann,
    Monoid ann
  ) =>
  TCEnv ann ->
  [(ResolvedDep Identifier, Constraint ann)] ->
  m (Maybe (Pattern ResolvedDep (Type ResolvedDep ann)))
getTypeForDictionary env constraints = do
  let getConstraintPattern (ident, constraint) = do
        (_, instanceExpr) <- lookupInstanceAndCheck env constraint
        let ty = getExprAnnotation instanceExpr
        pure (PVar ty ident)
  case constraints of
    [] -> pure Nothing
    [one] -> Just <$> getConstraintPattern one
    (one : rest) -> do
      pOne <- getConstraintPattern one
      pRest <- traverse getConstraintPattern (NE.fromList rest)
      let ty = TTuple mempty (getPatternAnnotation pOne) (getPatternAnnotation <$> pRest)
      pure $ Just $ PTuple ty pOne pRest

-- | 10x typeclasses implementation - given an `expr` that calls typeclass
-- methods, we inline all the instances as Let bindings
-- `let equals_1 = \a -> \b -> a == b in equals_1 10 11`
inlineTypeclassFunctions ::
  (MonadError (TCError ann) m, Ord ann, Show ann, Monoid ann) =>
  TCEnv ann ->
  M.Map (ResolvedDep Identifier) (Constraint ann) ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
inlineTypeclassFunctions env constraints expr = do
  let (dedupedConstraints, nameSwaps) = dedupeConstraints constraints
      tidyExpr = swapExprVarnames nameSwaps expr
  maybePattern <- getTypeForDictionary env dedupedConstraints
  case maybePattern of
    Just pat -> do
      let dictType = getPatternAnnotation pat
          wholeType = TFunc mempty mempty dictType (getExprAnnotation expr)
      pure $
        ELambda
          wholeType
          "instances"
          ( EPatternMatch
              (getExprAnnotation expr)
              (EAnn dictType (dictType $> dictType) (EVar dictType "instances"))
              (NE.fromList [(pat, tidyExpr)])
          )
    Nothing -> pure expr
