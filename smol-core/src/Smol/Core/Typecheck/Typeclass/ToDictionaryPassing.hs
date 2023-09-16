{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
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
    ToDictEnv (..),
    PassDictEnv (..),
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Char as Char
import Data.Foldable (foldl', traverse_)
import Data.Functor
import Data.List (elemIndex)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Smol.Core.ExprUtils
import Smol.Core.Helpers
import Smol.Core.Printer
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Typeclass.Deduplicate
import Smol.Core.Typecheck.Typeclass.Helpers
import Smol.Core.Typecheck.Types
import Smol.Core.Typecheck.Types.Substitution
import Smol.Core.Types

data ToDictEnv ann = ToDictEnv
  { tdeClasses :: M.Map TypeclassName (Typeclass ResolvedDep ann),
    tdeInstances :: M.Map (Constraint ResolvedDep ()) (Instance ResolvedDep (Type ResolvedDep ann)),
    tdeVars :: M.Map (ResolvedDep Identifier) ([Constraint ResolvedDep ann], ResolvedType ann)
  }

-- | Are we currently creating an instance? If so, include it's constraint
-- so it is able to refer to itself
newtype PassDictEnv = PassDictEnv
  { pdeCurrentConstraint :: Maybe (Constraint ResolvedDep ())
  }
  deriving newtype (Eq, Ord, Show)

emptyPassDictEnv :: PassDictEnv
emptyPassDictEnv = PassDictEnv Nothing

-- | the instances we've accumulated whilst traversing the expr
newtype PassDictState ann = PassDictState
  { pdsInstances :: M.Map (Constraint ResolvedDep ()) (Expr ResolvedDep (Type ResolvedDep ann))
  }
  deriving newtype (Eq, Ord, Show)

emptyPassDictState :: PassDictState ann
emptyPassDictState = PassDictState mempty

-- create an instance using the already typechecked instances we already have
lookupTypecheckedTypeclassInstance ::
  forall m ann.
  ( MonadError (TCError ann) m,
    MonadReader PassDictEnv m,
    Monoid ann,
    Ord ann,
    Show ann
  ) =>
  ToDictEnv ann ->
  Constraint ResolvedDep (Type ResolvedDep ann) ->
  m ([Substitution ResolvedDep ann], Instance ResolvedDep (Type ResolvedDep ann))
lookupTypecheckedTypeclassInstance env constraint = do
  let instances = tdeInstances env
  case M.lookup (void constraint) instances of
    Just tcInstance -> pure (mempty, tcInstance)
    Nothing -> do
      (foundConstraint, subs) <-
        findMatchingConstraint (M.keys instances) (removeTypesFromConstraint constraint)

      -- we can't actually provide an instance so what to do?
      -- a) look up main instance
      case M.lookup (void foundConstraint) instances of
        Just (Instance {inConstraints, inExpr}) -> do
          -- specialise contraints to found types
          let subbedConstraints =
                substituteConstraint subs
                  . removeTypesFromConstraint
                  <$> inConstraints

          -- see if found types exist
          traverse_
            (lookupTypecheckedTypeclassInstance env)
            (addTypesToConstraint <$> subbedConstraints)

          -- return new instance
          pure
            ( subs,
              Instance
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
    MonadReader PassDictEnv m,
    Monoid ann,
    Ord ann,
    Show ann
  ) =>
  ToDictEnv ann ->
  [Constraint ResolvedDep (Type ResolvedDep ann)] ->
  m (Maybe (Pattern ResolvedDep (Type ResolvedDep ann)))
getTypeForDictionary env constraints = do
  let getConstraintPattern constraint i = do
        let ident = identForConstraint (i + 1)
        result <- tryError (lookupTypecheckedTypeclassInstance env constraint)
        ty <- case result of
          -- we found the instance, return it's type
          Right (_, Instance _ instanceExpr) -> pure (getExprAnnotation instanceExpr)
          -- we didn't find an instance, but we can get the type from the
          -- constraint
          Left e -> case typeForConstraint (tdeClasses env) constraint of
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
typeForConstraint :: M.Map TypeclassName (Typeclass ResolvedDep ann) -> Constraint ResolvedDep (Type ResolvedDep ann) -> Maybe (Type ResolvedDep ann)
typeForConstraint typeClasses constraint@(Constraint tcn _) = do
  M.lookup tcn typeClasses
    <&> \typeclass -> applyConstraintTypes typeclass constraint

-- | 10x typeclasses implementation - given an `expr` that calls typeclass
-- methods, we inline all the instances as Let bindings
-- `let equals_1 = \a -> \b -> a == b in equals_1 10 11`
convertExprToUseTypeclassDictionary ::
  ( MonadError (TCError ann) m,
    MonadReader PassDictEnv m,
    Monoid ann,
    Ord ann,
    Show ann
  ) =>
  ToDictEnv ann ->
  [Constraint ResolvedDep (Type ResolvedDep ann)] ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
convertExprToUseTypeclassDictionary env constraints expr = do
  -- if our constraints are concrete we'll inline them rather than passing them
  -- through, as such
  maybePattern <- getTypeForDictionary env (filterNotConcrete constraints)

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
  ( Show ann,
    Ord ann,
    Monoid ann,
    MonadReader PassDictEnv m,
    MonadError (TCError ann) m
  ) =>
  ToDictEnv ann ->
  NE.NonEmpty (Constraint ResolvedDep (Type ResolvedDep ann)) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
createTypeclassDict env constraints = do
  foundInstances <-
    traverse
      ( \constraint -> do
          result <- tryError (lookupTypecheckedTypeclassInstance env constraint)
          case result of
            Right (subs, Instance newConstraints expr) -> do
              -- found a concrete instance
              toDictionaryPassingInternal env subs newConstraints expr
            Left e -> do
              -- no concrete instance, maybe we can pass through a constraint
              -- from the current function
              case (,)
                <$> elemIndex (removeTypesFromConstraint constraint) (removeTypesFromConstraint <$> NE.toList constraints)
                <*> typeForConstraint (tdeClasses env) constraint of
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

storeInstance ::
  (MonadState (PassDictState ann) m) =>
  Constraint ResolvedDep () ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (ResolvedDep Identifier)
storeInstance constraint instanceExpr = do
  modify (\pds -> pds {pdsInstances = pdsInstances pds <> M.singleton constraint instanceExpr})
  pure (identifierFromConstraint constraint)

lookupInstance ::
  ( MonadReader PassDictEnv m,
    MonadState (PassDictState ann) m
  ) =>
  Constraint ResolvedDep () ->
  m (Maybe (ResolvedDep Identifier))
lookupInstance constraint = do
  maybeCurrentConstraint <- asks pdeCurrentConstraint
  if maybeCurrentConstraint == Just constraint
    then pure $ Just $ identifierFromConstraint constraint -- this is the current instance, so return its own name
    else do
      maybeInstance <- gets (M.lookup constraint . pdsInstances)
      case maybeInstance of
        Just _ -> pure $ Just $ identifierFromConstraint constraint
        Nothing -> pure Nothing

-- make a nice name for an instance
identifierFromConstraint :: Constraint ResolvedDep () -> ResolvedDep Identifier
identifierFromConstraint (Constraint (TypeclassName tcn) tys) = LocalDefinition . Identifier $ toBasic $ tcn <> "_" <> foldMap tshowType tys
  where
    toBasic :: T.Text -> T.Text
    toBasic = T.toLower . T.filter Char.isAlpha

    tshowType :: Type ResolvedDep ann -> T.Text
    tshowType ty =
      renderWithWidth 100 (prettyDoc ty)

-- given we know the types of all our deps
-- pass dictionaries to them all
passDictionaries ::
  ( Monoid ann,
    Ord ann,
    Show ann,
    MonadReader PassDictEnv m,
    MonadError (TCError ann) m
  ) =>
  ToDictEnv ann ->
  [Substitution ResolvedDep ann] ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
passDictionaries env subs expr = do
  (finalExpr, dictState) <-
    flip
      runStateT
      emptyPassDictState
      (go expr)
  pure $
    foldl'
      ( \totalExpr (constraint, instanceExpr) ->
          ELet (TPrim mempty TPBool) (identifierFromConstraint constraint) instanceExpr totalExpr
      )
      finalExpr
      (M.toList $ pdsInstances dictState)
  where
    go (EVar ann ident) =
      case M.lookup ident (tdeVars env) of
        Just (constraints, _defExpr) -> do
          -- need to specialise constraint to actual type here
          case NE.nonEmpty constraints of
            Just neConstraints -> do
              -- use the call type to specialise to the instance we need
              specialisedConstraints <- traverse (specialiseConstraint (tdeClasses env) ann) neConstraints
              EApp ann (EVar ann ident) <$> createTypeclassDict env (addTypesToConstraint <$> specialisedConstraints)
            Nothing -> pure (EVar ann ident)
        Nothing -> do
          result <- recoverInstance (tdeClasses env) ident ann
          case result of
            Just constraint -> do
              -- specialise contraints to found types
              let subbedConstraint =
                    substituteConstraint subs constraint

              -- have we already created this instance?
              maybeFound <- lookupInstance (void subbedConstraint)
              case maybeFound of
                -- if so, return it
                Just identifier -> pure (EVar ann identifier)
                Nothing -> do
                  (newSubs, Instance fnConstraints fnExpr) <-
                    lookupTypecheckedTypeclassInstance env (addTypesToConstraint subbedConstraint)

                  -- convert instance to dictionary passing then return it inlined
                  toInline <-
                    local
                      (const (PassDictEnv $ Just $ void subbedConstraint))
                      (toDictionaryPassingInternal env newSubs fnConstraints fnExpr)

                  -- need to push this to state with a fresh name, and put a var to
                  -- the fresh name
                  identifier <- storeInstance (void subbedConstraint) toInline

                  pure (EVar ann identifier)
            Nothing -> pure (EVar ann ident)
    go other = bindExpr go other

-- | well well well lets put it all together
toDictionaryPassingInternal ::
  ( MonadError (TCError ann) m,
    MonadReader PassDictEnv m,
    Show ann,
    Ord ann,
    Monoid ann
  ) =>
  ToDictEnv ann ->
  [Substitution ResolvedDep ann] ->
  [Constraint ResolvedDep (Type ResolvedDep ann)] ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
toDictionaryPassingInternal env subs constraints expr = do
  passDictionaries env subs
    <=< convertExprToUseTypeclassDictionary env constraints
    $ expr

-- | well well well lets put it all together
toDictionaryPassing ::
  (MonadError (TCError ann) m, Show ann, Ord ann, Monoid ann) =>
  ToDictEnv ann ->
  [Substitution ResolvedDep ann] ->
  [Constraint ResolvedDep (Type ResolvedDep ann)] ->
  Expr ResolvedDep (Type ResolvedDep ann) ->
  m (Expr ResolvedDep (Type ResolvedDep ann))
toDictionaryPassing env subs constraints expr =
  runReaderT (toDictionaryPassingInternal env subs constraints expr) emptyPassDictEnv
