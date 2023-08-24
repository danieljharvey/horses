{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smol.Core.Typecheck.Typeclass.ToDictionaryPassing
  ( convertExprToUseTypeclassDictionary,
    getTypeclassMethodNames,
    createTypeclassDict,
    toDictionaryPassing,
    passDictionaries,
    lookupTypecheckedTypeclassInstance,
  )
where

import Control.Monad
import Control.Monad.Except
import Data.Foldable (traverse_)
import Data.Functor
import Data.List (elemIndex)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Smol.Core.ExprUtils
import Smol.Core.Helpers
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Typeclass.Deduplicate
import Smol.Core.Typecheck.Typeclass.Helpers
import Smol.Core.Typecheck.Types
import Smol.Core.Typecheck.Types.Substitution
import Smol.Core.Types

-- create an instance using the already typechecked instances we already have
lookupTypecheckedTypeclassInstance ::
  forall m ann.
  (MonadError (TCError ann) m, Monoid ann, Ord ann, Show ann) =>
  TCEnv ann ->
  M.Map (Constraint ResolvedDep ()) (Instance ResolvedDep (Type ResolvedDep ann)) ->
  Constraint ResolvedDep (Type ResolvedDep ann) ->
  m (Instance ResolvedDep (Type ResolvedDep ann))
lookupTypecheckedTypeclassInstance env instances constraint = do
  tracePrettyM "lookupTypecheckedTypeclassInstance" constraint
  case M.lookup (void constraint) instances of
    Just tcInstance -> pure tcInstance
    Nothing -> do
      (foundConstraint, subs) <- findMatchingConstraint (M.keys instances) (removeTypesFromConstraint constraint)
      tracePrettyM "foundConstraint" foundConstraint
      tracePrettyM "subs" subs

      -- we can't actually provide an instance so what to do?
      -- a) look up main instance
      case M.lookup (void foundConstraint) instances of
        Just (Instance {inConstraints, inExpr}) -> do
          tracePrettyM "found concrete generalised instance" (inConstraints, inExpr)
          -- specialise contraints to found types
          let subbedConstraints =
                substituteConstraint subs
                  . removeTypesFromConstraint
                  <$> inConstraints

          tracePrettyM "subbed constraints" subbedConstraints

          -- see if found types exist
          traverse_ (lookupTypecheckedTypeclassInstance env instances) (addTypesToConstraint <$> subbedConstraints)

          -- return new instance
          pure
            ( Instance
                { inConstraints = addTypesToConstraint <$> subbedConstraints,
                  inExpr
                }
            )
        Nothing ->
          let constraintsWithAnn = (fmap . fmap) (const mempty) (M.keys instances)
              (Constraint name tys) = constraint
           in throwError (TCTypeclassInstanceNotFound name (getTypeAnnotation <$> tys) constraintsWithAnn)

-- | given a pile of constraints, find the matching one and return
-- substitutions required to make it match
-- TODO: this needs to accept TCEnv and lookup constraints in there too thx
-- TODO: make our own error types for this crap so it's less confusing what is
-- a type error or not
findMatchingConstraint ::
  forall m ann.
  (MonadError (TCError ann) m, Monoid ann) =>
  [Constraint ResolvedDep ()] ->
  Constraint ResolvedDep ann ->
  m (Constraint ResolvedDep ann, [Substitution ResolvedDep ann])
findMatchingConstraint constraints (Constraint name tys) =
  let constraintsWithAnn :: [Constraint ResolvedDep ann]
      constraintsWithAnn = (fmap . fmap) (const mempty) constraints

      lookupConstraint (Constraint innerName innerTys) =
        case (innerName == name, instanceMatchesType tys innerTys) of
          (True, Right matches) -> Just (Constraint innerName innerTys, matches)
          _ -> Nothing
   in case mapMaybe lookupConstraint constraintsWithAnn of
        -- we deliberately fail if we find more than one matching instance
        [(foundConstraint, subs)] -> pure (foundConstraint, subs)
        [] ->
          throwError (TCTypeclassInstanceNotFound name tys constraintsWithAnn)
        multiple ->
          throwError (TCConflictingTypeclassInstancesFound (fst <$> multiple))

getTypeForDictionary ::
  ( MonadError (TCError ann) m,
    Monoid ann,
    Ord ann,
    Show ann
  ) =>
  TCEnv ann ->
  M.Map (Constraint ResolvedDep ()) (Instance ResolvedDep (Type ResolvedDep ann)) ->
  [Constraint ResolvedDep (Type ResolvedDep ann)] ->
  m (Maybe (Pattern ResolvedDep (Type ResolvedDep ann)))
getTypeForDictionary env instances constraints = do
  let getConstraintPattern constraint i = do
        let ident = identForConstraint (i + 1)
        ty <- case lookupTypecheckedTypeclassInstance env instances constraint of
          -- we found the instance, return it's type
          Right (Instance _ instanceExpr) -> pure (getExprAnnotation instanceExpr)
          -- we didn't find an instance, but we can get the type from the
          -- constraint
          Left e -> case typeForConstraint env constraint of
            Just ty -> pure ty
            Nothing -> throwError e
        pure (PVar ty ident)

  case constraints of
    [] -> pure Nothing
    [one] -> Just <$> getConstraintPattern one (-1)
    (one : rest) -> do
      pOne <- getConstraintPattern one (-1)
      pRest <- NE.fromList <$> traverseInd getConstraintPattern rest
      let ty = TTuple mempty (getPatternAnnotation pOne) (getPatternAnnotation <$> pRest)
      pure $ Just $ PTuple ty pOne pRest

-- | when typechecking instances we can look them up and literally typecheck
-- them, however for constraints we don't have concrete code yet
-- however, we can just substitute the types from the Constraint to the Typeclass
-- to see what type we should get
typeForConstraint :: TCEnv ann -> Constraint ResolvedDep (Type ResolvedDep ann) -> Maybe (Type ResolvedDep ann)
typeForConstraint env constraint@(Constraint tcn _) = do
  M.lookup tcn (tceClasses env)
    <&> \typeclass -> applyConstraintTypes typeclass constraint

-- | 10x typeclasses implementation - given an `expr` that calls typeclass
-- methods, we inline all the instances as Let bindings
-- `let equals_1 = \a -> \b -> a == b in equals_1 10 11`
convertExprToUseTypeclassDictionary ::
  (MonadError (TCError ann) m, Monoid ann, Ord ann, Show ann) =>
  TCEnv ann ->
  M.Map (Constraint ResolvedDep ()) (Instance ResolvedDep (Type ResolvedDep ann)) ->
  [Constraint ResolvedDep (Type ResolvedDep ann)] ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
convertExprToUseTypeclassDictionary env instances constraints expr = do
  tracePrettyM "convertExprToUseTypeclassDictionary" (constraints, expr)

  maybePattern <- getTypeForDictionary env instances (filterNotConcrete constraints)

  case maybePattern of
    Just pat -> do
      let dictType = getPatternAnnotation pat
          exprType = getExprAnnotation expr
      pure $
        ELambda
          (TFunc mempty mempty dictType exprType)
          "instances"
          ( EPatternMatch
              (getExprAnnotation expr)
              (EAnn dictType (dictType $> dictType) (EVar dictType "instances"))
              (NE.fromList [(pat, expr)])
          )
    Nothing -> pure expr

-- | create a typeclass dictionary
-- return either solid instances or use vars from constraints if not available
-- (ie "pass them through", as such)
createTypeclassDict ::
  (Show ann, Ord ann, Monoid ann, MonadError (TCError ann) m) =>
  TCEnv ann ->
  M.Map (Constraint ResolvedDep ()) (Instance ResolvedDep (Type ResolvedDep ann)) ->
  NE.NonEmpty (Constraint ResolvedDep (Type ResolvedDep ann)) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
createTypeclassDict env instances constraints = do
  foundInstances <-
    traverse
      ( \constraint -> do
          case lookupTypecheckedTypeclassInstance env instances constraint of
            Right (Instance newConstraints expr) -> do
              tracePrettyM "createTypeclassDict" (NE.toList constraints, newConstraints, expr)
              -- add instance contraints to typecheck env
              let newEnv = env {tceConstraints = removeTypesFromConstraint <$> newConstraints}
              -- found a concrete instance
              toDictionaryPassing newEnv instances newConstraints expr
            Left e -> do
              -- no concrete instance, maybe we can pass through a constraint
              -- from the current function
              case (,) <$> elemIndex (removeTypesFromConstraint constraint) (tceConstraints env) <*> typeForConstraint env constraint of
                Just (index, ty) -> pure (EVar ty (identForConstraint $ fromIntegral index))
                Nothing -> throwError e
      )
      constraints
  case NE.uncons foundInstances of
    (one, Nothing) -> pure one
    (theFirst, Just theRest) ->
      let ty = TTuple mempty (getExprAnnotation theFirst) (getExprAnnotation <$> theRest)
       in pure $ ETuple ty theFirst theRest

filterNotConcrete :: [Constraint ResolvedDep ann] -> [Constraint ResolvedDep ann]
filterNotConcrete = filter (not . isConcrete)

-- given we know the types of all our deps
-- pass dictionaries to them all
passDictionaries ::
  (Monoid ann, Ord ann, Show ann, MonadError (TCError ann) m) =>
  TCEnv ann ->
  M.Map (Constraint ResolvedDep ()) (Instance ResolvedDep (Type ResolvedDep ann)) ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
passDictionaries env instances =
  go
  where
    go (EVar ann ident) = do
      tracePrettyM "passDictionaries to " ident
      tracePrettyM "tce vars" (tceVars env)
      case M.lookup ident (tceVars env) of
        Just (constraints, _defExpr) -> do
          tracePrettyM "found in vars" (constraints, ident)
          -- need to specialise constraint to actual type here
          case NE.nonEmpty constraints of
            Just neConstraints -> do
              -- use the call type to specialise to the instance we need
              specialisedConstraints <- traverse (specialiseConstraint env ann) neConstraints
              EApp ann (EVar ann ident) <$> createTypeclassDict env instances (addTypesToConstraint <$> specialisedConstraints)
            Nothing -> pure (EVar ann ident)
        Nothing -> do
          tracePrettyM "not found in vars" ident
          result <- recoverInstance env ident ann
          case result of
            Just constraint -> do
              tracePrettyM "recovered an instance" constraint
              tracePrettyM "constraints in env" (tceConstraints env)
              (Instance fnConstraints fnExpr) <- liftEither (lookupTypecheckedTypeclassInstance env instances (addTypesToConstraint constraint))
              tracePrettyM "found instance" (fnConstraints, fnExpr)
              -- convert instance to dictionary passing then return it inlined
              toDictionaryPassing env instances fnConstraints fnExpr
            Nothing ->
              pure (EVar ann ident)
    go other = bindExpr go other

-- | well well well lets put it all together
toDictionaryPassing ::
  (MonadError (TCError ann) m, Show ann, Ord ann, Monoid ann) =>
  TCEnv ann ->
  M.Map (Constraint ResolvedDep ()) (Instance ResolvedDep (Type ResolvedDep ann)) ->
  [Constraint ResolvedDep (Type ResolvedDep ann)] ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
toDictionaryPassing env instances constraints expr = do
  tracePrettyM "toDictionaryPassing" (tceConstraints env, constraints, expr)

  -- initial typechecking environment
  let typecheckEnv =
        env
          { tceConstraints = removeTypesFromConstraint <$> constraints
          }

  tracePrettyM "toDictionaryPassing env constraints" (tceConstraints typecheckEnv)

  passDictionaries typecheckEnv instances
    <=< convertExprToUseTypeclassDictionary typecheckEnv instances constraints
    $ expr
