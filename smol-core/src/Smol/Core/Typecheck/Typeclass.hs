{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Typeclass
  ( checkInstance,
    lookupInstanceAndCheck,
    convertExprToUseTypeclassDictionary,
    getTypeclassMethodNames,
    createTypeclassDict,
    passAllDictionaries,
    passDictionaries,
    passOuterDictionaries,
    module Smol.Core.Typecheck.Typeclass.Helpers,
    module Smol.Core.Typecheck.Typeclass.Deduplicate,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Monoid
import qualified Data.Set as S
import Smol.Core.ExprUtils
import Smol.Core.Helpers
import Smol.Core.Modules.ResolveDeps
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Typecheck (typecheck)
import Smol.Core.Typecheck.Typeclass.BuiltIns
import Smol.Core.Typecheck.Typeclass.Deduplicate
import Smol.Core.Typecheck.Typeclass.Helpers
import Smol.Core.Typecheck.Types
import Smol.Core.Types

toParseExpr :: Expr Identity ann -> Expr ParseDep ann
toParseExpr = mapExprDep resolve
  where
    resolve (Identity a) = emptyParseDep a

resolveType :: Type Identity ann -> Type ResolvedDep ann
resolveType = mapTypeDep resolve
  where
    resolve (Identity a) = emptyResolvedDep a

lookupInstanceAndCheck ::
  (Ord ann, Monoid ann, Show ann, MonadError (TCError ann) m) =>
  TCEnv ann ->
  Constraint ann ->
  m
    ( Identifier,
      [Constraint ann],
      Expr ResolvedDep (Type ResolvedDep ann)
    )
lookupInstanceAndCheck env tch@(Constraint typeclassName _) = do
  tcInstance <- lookupTypeclassInstance env tch
  typeclass <- case M.lookup typeclassName (tceClasses env) of
    Just tc -> pure tc
    Nothing -> error "fuck"
  checkInstance env typeclass tch tcInstance

applyConstraintTypes :: Typeclass ann -> Constraint ann -> Type Identity ann
applyConstraintTypes (Typeclass _ args _ ty) (Constraint _ tys) =
  let subs =
        ( \(ident, tySub) ->
            Substitution (SubId $ Identity ident) tySub
        )
          <$> zip args tys
   in substituteMany subs ty

checkInstance ::
  (MonadError (TCError ann) m, Monoid ann, Ord ann, Show ann) =>
  TCEnv ann ->
  Typeclass ann ->
  Constraint ann ->
  Instance ann ->
  m (Identifier, [Constraint ann], Expr ResolvedDep (Type ResolvedDep ann))
checkInstance tcEnv typeclass constraint (Instance constraints expr) =
  do
    let subbedType = applyConstraintTypes typeclass constraint
        funcName = tcFuncName typeclass

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

    case resolveExprDeps (toParseExpr annotatedExpr) typeclassMethodNames of
      Left resolveErr -> error $ "Resolve error: " <> show resolveErr
      Right resolvedExpr -> do
        (newConstraints, typedExpr) <- typecheck typecheckEnv resolvedExpr

        pure (funcName, newConstraints, typedExpr)

-- let's get all the method names from the Typeclasses
-- mentioned in the instance constraints
getTypeclassMethodNames :: TCEnv ann -> S.Set Identifier
getTypeclassMethodNames tcEnv =
  S.fromList $
    tcFuncName <$> M.elems (tceClasses tcEnv)

createInstance ::
  (MonadError (TCError ann) m, Ord ann, Show ann, Monoid ann) =>
  TCEnv ann ->
  [Constraint ann] ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
createInstance env constraints typedExpr = do
  -- if this has any constraints of it's own, convert to dictionary-receiving
  -- style
  dictExpr <- convertExprToUseTypeclassDictionary env constraints typedExpr

  -- pass dictionaries to it
  passOuterDictionaries env constraints dictExpr

getTypeForDictionary ::
  ( MonadError (TCError ann) m,
    Ord ann,
    Show ann,
    Monoid ann
  ) =>
  TCEnv ann ->
  [Constraint ann] ->
  m (Maybe (Pattern ResolvedDep (Type ResolvedDep ann)))
getTypeForDictionary env constraints = do
  let getConstraintPattern constraint i = do
        let ident = identForConstraint (i + 1)
        result <- runExceptT $ lookupInstanceAndCheck env constraint
        ty <- case result of
          -- we found the instance, return it's type
          Right (_, _, instanceExpr) -> pure (getExprAnnotation instanceExpr)
          -- we didn't find an instance, but we can get the type from the
          -- constraint
          Left e -> case typeForConstraint env constraint of
            Just ty -> pure (resolveType ty)
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
typeForConstraint :: TCEnv ann -> Constraint ann -> Maybe (Type Identity ann)
typeForConstraint env constraint@(Constraint tcn _) = do
  M.lookup tcn (tceClasses env)
    <&> \typeclass -> applyConstraintTypes typeclass constraint

-- | 10x typeclasses implementation - given an `expr` that calls typeclass
-- methods, we inline all the instances as Let bindings
-- `let equals_1 = \a -> \b -> a == b in equals_1 10 11`
convertExprToUseTypeclassDictionary ::
  (MonadError (TCError ann) m, Ord ann, Show ann, Monoid ann) =>
  TCEnv ann ->
  [Constraint ann] ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
convertExprToUseTypeclassDictionary env constraints expr = do
  maybePattern <- getTypeForDictionary env constraints

  newExpr <- case maybePattern of
    Just pat -> do
      let dictType = getPatternAnnotation pat
          exprType = getExprAnnotation expr
      pure $
        ELambda
          exprType -- we want the overall expression to have the same type so we can still typecheck and ignore the constraints
          "instances"
          ( EPatternMatch
              (getExprAnnotation expr)
              (EAnn dictType (dictType $> dictType) (EVar dictType "instances"))
              (NE.fromList [(pat, expr)])
          )
    Nothing -> pure expr

  pure newExpr

-- | create a typeclass dictionary
-- if any of the types are non-concrete (ie, `Eq a`)
-- then return Nothing - these aren't ready to be applied
createTypeclassDict ::
  (Show ann, Ord ann, Monoid ann, MonadError (TCError ann) m) =>
  TCEnv ann ->
  [Constraint ann] ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
createTypeclassDict env constraints = do
  instances <-
    traverse
      ( \constraint -> do
          (_, newConstraints, expr) <- lookupInstanceAndCheck env constraint
          createInstance env newConstraints expr
      )
      constraints
  case instances of
    [] -> error "what the fuck man, no constraints"
    [one] -> pure one
    (theFirst : theRest) ->
      let ty = TTuple mempty (getExprAnnotation theFirst) (NE.fromList $ getExprAnnotation <$> theRest)
       in pure $ ETuple ty theFirst (NE.fromList theRest)

constraintsAreAllConcrete :: [Constraint ann] -> Bool
constraintsAreAllConcrete =
  getAll
    . foldMap (All . isConcrete)

-- given we know the types of all our deps
-- pass dictionaries to them all
passDictionaries ::
  (Monoid ann, Ord ann, Show ann, MonadError (TCError ann) m) =>
  TCEnv ann ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
passDictionaries env =
  go
  where
    go (EVar ann ident) =
      case M.lookup ident (tceVars env) of
        Just (constraints, _defExpr) -> do
          if constraintsAreAllConcrete constraints
            then do
              dict <- createTypeclassDict env constraints

              pure (EApp ann (EVar ann ident) dict)
            else pure (EVar ann ident)
        Nothing -> pure (EVar ann ident)
    go other = bindExpr go other

-- pass dictionaries to the current expression
-- TODO: I don't think this should happen ever
-- we should be inlining concrete instances instead
passOuterDictionaries ::
  (Monoid ann, Ord ann, Show ann, MonadError (TCError ann) m) =>
  TCEnv ann ->
  [Constraint ann] ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
passOuterDictionaries _ [] expr = pure expr
passOuterDictionaries env constraints expr = do
  if constraintsAreAllConcrete constraints
    then do
      dict <- createTypeclassDict env constraints
      let ann = getExprAnnotation expr
      pure (EApp ann expr dict)
    else pure expr

-- | well well well lets put it all together
passAllDictionaries ::
  (MonadError (TCError ann) m, Show ann, Ord ann, Monoid ann) =>
  [Constraint ann] ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
passAllDictionaries constraints expr = do
  tracePrettyM "expr" expr
  tracePrettyM "constraints" constraints

  -- initial typechecking environment
  let env =
        TCEnv
          { tceVars = mempty,
            tceDataTypes = mempty,
            tceClasses = builtInClasses,
            tceInstances = builtInInstances,
            tceConstraints = constraints
          }

  newExpr <-
    passOuterDictionaries env constraints
      <=< passDictionaries env
      <=< convertExprToUseTypeclassDictionary env constraints
      $ expr

  tracePrettyM "newExpr" newExpr

  pure newExpr
