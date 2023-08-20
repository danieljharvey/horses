{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Smol.Core.Typecheck.Typeclass.Helpers
  ( recoverTypeclassUses,
    constraintsFromTLE,
    lookupTypeclassConstraint,
    lookupTypeclassInstance,
    matchType,
    lookupTypeclass,
    instanceMatchesType,
    isConcrete,
    recoverInstance,
    specialiseConstraint,
    envFromTypecheckedModule,
  )
where

import Control.Monad (unless, void, zipWithM)
import Control.Monad.Except
import Control.Monad.Writer
import Data.Foldable (traverse_)
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid
import Smol.Core.Helpers
import Smol.Core.Modules.Types
import Smol.Core.TypeUtils
import Smol.Core.Typecheck.Shared
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Subtype
import Smol.Core.Typecheck.Typeclass.BuiltIns
import Smol.Core.Typecheck.Types
import Smol.Core.Types

-- this just chucks types in any order and will break on multi-parameter type
-- classes
recoverTypeclassUses ::
  (Monoid ann) =>
  [TCWrite ann] ->
  M.Map (ResolvedDep Identifier) (Constraint ResolvedDep ann)
recoverTypeclassUses events =
  let allSubs = filterSubstitutions events
      allTCs = filterTypeclassUses events
      substituteMatch (ident, unknownId) =
        (ident, substituteMany allSubs (TUnknown mempty unknownId))
      fixTC (identifier, name, matches) =
        (identifier, name, substituteMatch <$> matches)
      toConstraint (identifier, name, fixedMatches) =
        M.singleton identifier (Constraint name (snd <$> fixedMatches))
   in mconcat $ toConstraint . fixTC <$> allTCs

-- thing we're matching, typeclass we're checking
-- pretty sure this is still incomplete
matchType ::
  (Eq (dep TypeName)) =>
  Type dep ann ->
  Type dep ann ->
  Either
    (Type dep ann, Type dep ann)
    [Substitution dep ann]
matchType ty (TVar _ ident) =
  Right [Substitution (SubId ident) ty]
matchType (TTuple _ a as) (TTuple _ b bs) = do
  match <- matchType a b
  matches <- zipWithM matchType (NE.toList as) (NE.toList bs)
  pure (match <> mconcat matches)
matchType (TConstructor _ conA) (TConstructor _ conB) | conA == conB = do
  pure mempty
matchType (TApp _ lFn lArg) (TApp _ rFn rArg) = do
  matchA <- matchType lFn rFn
  matchB <- matchType lArg rArg
  pure (matchA <> matchB)
matchType (TArray _ _ a) (TArray _ _ b) =
  matchType a b
matchType a b = Left (a, b)

instanceMatchesType ::
  (Eq (dep TypeName)) =>
  [Type dep ann] ->
  [Type dep ann] ->
  Either
    (Type dep ann, Type dep ann)
    [Substitution dep ann]
instanceMatchesType needleTys haystackTys =
  mconcat <$> zipWithM matchType needleTys haystackTys

-- | wipe out annotations when looking for instances
-- this is fragile and depends on us manually creating instances with `mempty`
-- annotations in the first place
lookupConcreteInstance ::
  (Monoid ann, Ord ann) =>
  TCEnv ann ->
  Constraint ResolvedDep ann ->
  Maybe (Instance ResolvedDep ann)
lookupConcreteInstance env constraint =
  M.lookup (constraint $> mempty) (tceInstances env)

-- | do we have a matching instance? if we're looking for a concrete type and
-- it's not there, explode (ie, there is no `Eq Bool`)
-- or return it
lookupTypeclassInstance ::
  (MonadError (TCError ann) m, Monoid ann, Ord ann, Show ann) =>
  TCEnv ann ->
  Constraint ResolvedDep ann ->
  m (Instance ResolvedDep ann)
lookupTypeclassInstance env constraint@(Constraint name tys) = do
  -- first, do we have a concrete instance?
  case lookupConcreteInstance env constraint of
    Just tcInstance -> pure tcInstance
    Nothing -> do
      case mapMaybe
        ( \(Constraint innerName innerTys) ->
            case (innerName == name, instanceMatchesType tys innerTys) of
              (True, Right matches) -> Just (Constraint innerName innerTys, matches)
              _ -> Nothing
        )
        (M.keys (tceInstances env)) of
        -- we deliberately fail if we find more than one matching instance
        [(foundConstraint, subs)] -> do
          -- a) look up main instance
          case lookupConcreteInstance env foundConstraint of
            Just (Instance {inConstraints, inExpr}) -> do
              -- specialise contraints to found types
              let subbedConstraints = substituteConstraint subs <$> inConstraints
              -- see if found types exist
              traverse_ (lookupTypeclassInstance env) subbedConstraints
              -- return new instance
              pure (Instance {inConstraints = subbedConstraints, inExpr})
            Nothing ->
              throwError (TCTypeclassInstanceNotFound name tys (M.keys $ tceInstances env))
        [] ->
          throwError (TCTypeclassInstanceNotFound name tys (M.keys $ tceInstances env))
        multiple ->
          throwError (TCConflictingTypeclassInstancesFound (fst <$> multiple))

substituteConstraint ::
  (Eq (dep Identifier)) =>
  [Substitution dep ann] ->
  Constraint dep ann ->
  Constraint dep ann
substituteConstraint subs (Constraint name tys) =
  Constraint name (substituteMany subs <$> tys)

-- | do we have a matching constraint?
-- first look for a concrete instance
-- if one is not there, see if we already have a matching constraint
-- in TCEnv (ie, the function has declared `Eq a`)
lookupTypeclassConstraint ::
  (MonadError (TCError ann) m, Ord ann, Monoid ann, Show ann) =>
  TCEnv ann ->
  Constraint ResolvedDep ann ->
  m ()
lookupTypeclassConstraint env constraint@(Constraint name tys) = do
  -- see if this is a valid instance first?
  _ <-
    void (lookupTypeclassInstance env constraint) `catchError` \_ -> do
      -- maybe it's a constraint, look there
      unless
        (constraint `elem` tceConstraints env)
        (throwError (TCTypeclassInstanceNotFound name tys (M.keys $ tceInstances env)))
  pure ()

-- look for vars, if no, then it's concrete
isConcrete :: Constraint ResolvedDep ann -> Bool
isConcrete (Constraint _ tys) =
  not $ getAny $ foldMap containsVars tys
  where
    containsVars (TVar {}) = Any True
    containsVars other = monoidType containsVars other

-- given a func name and type, find the typeclass instance (if applicable)
recoverInstance ::
  (MonadError (TCError ann) m, Eq ann, Show ann, Monoid ann) =>
  TCEnv ann ->
  ResolvedDep Identifier ->
  Type ResolvedDep ann ->
  m (Maybe (Constraint ResolvedDep ann))
recoverInstance env ident ty = do
  let getInnerIdent (TypeclassCall i _) = Just i
      getInnerIdent (LocalDefinition i) = Just i -- not sure if this should happen but it makes testing waaaay easier
      getInnerIdent _ = Nothing

  -- if name matches typeclass instance, return freshened type
  case listToMaybe $ M.elems $ M.filter (\tc -> Just (tcFuncName tc) == getInnerIdent ident) (tceClasses env) of
    -- need to turn Type Identity ann into Type ResolvedDep ann
    Just tc -> Just <$> applyTypeToConstraint tc ty
    Nothing -> pure Nothing

-- find Typeclass in env or explode
lookupTypeclass ::
  (MonadError (TCError ann) m) =>
  TCEnv ann ->
  TypeclassName ->
  m (Typeclass ResolvedDep ann)
lookupTypeclass env tcn =
  case M.lookup tcn (tceClasses env) of
    Just tc -> pure tc
    Nothing -> throwError (TCTypeclassNotFound tcn)

-- given a Typeclass (ie `Eq a`) and a type calling it (ie `Int -> Int ->
-- Bool`), recover the instance we want, `Eq Int`.
applyTypeToConstraint ::
  (Monoid ann, Show ann, Eq ann, MonadError (TCError ann) m) =>
  Typeclass ResolvedDep ann ->
  Type ResolvedDep ann ->
  m (Constraint ResolvedDep ann)
applyTypeToConstraint tc ty = do
  (_, subs) <- runWriterT $ isSubtypeOf (tcFuncType tc) ty
  let applySubs = substituteMany (filterSubstitutions subs) . TVar mempty . emptyResolvedDep
  pure $ Constraint (tcName tc) (applySubs <$> tcArgs tc)

-- given I have a constraint and a type for it's callsite
-- substitute the type onto the constraint to get the actual constraint.
-- For instance, I am calling a function `useEquals` that has constraint `Eq a`
-- with the values `(1 : Int) (2: Int)`. Therefore my type is `Int -> Int ->
-- Bool` and I can use that to specialise the constraint to `Eq Int` and thus
-- dispatch the correct `Eq` instance
specialiseConstraint ::
  (MonadError (TCError ann) m, Eq ann, Show ann, Monoid ann) =>
  TCEnv ann ->
  Type ResolvedDep ann ->
  Constraint ResolvedDep ann ->
  m (Constraint ResolvedDep ann)
specialiseConstraint env ty (Constraint tcn _tys) = do
  -- lookup typeclass
  tc <- lookupTypeclass env tcn
  -- apply types
  applyTypeToConstraint tc ty

constraintsFromTLE ::
  TopLevelExpression ResolvedDep (Type ResolvedDep ann) ->
  [Constraint ResolvedDep ann]
constraintsFromTLE tle =
  (fmap . fmap) getTypeAnnotation (tleConstraints tle)

-- get input for typechecker from module
getVarsInScope ::
  Module ResolvedDep (Type ResolvedDep ann) ->
  M.Map (ResolvedDep Identifier) ([Constraint ResolvedDep ann], ResolvedType ann)
getVarsInScope =
  M.fromList
    . fmap go
    . M.toList
    . moExpressions
  where
    go (ident, tle) =
      ( LocalDefinition ident,
        (constraintsFromTLE tle, getExprAnnotation (tleExpr tle))
      )

envFromTypecheckedModule :: (Ord ann, Monoid ann) => Module ResolvedDep (Type ResolvedDep ann) -> TCEnv ann
envFromTypecheckedModule inputModule =
  let instances =
        mapKey (fmap (const mempty))
          . (fmap . fmap) getTypeAnnotation
          . moInstances
          $ inputModule

      classes = (fmap . fmap) getTypeAnnotation (moClasses inputModule)

      dataTypes =
        (fmap . fmap)
          getTypeAnnotation
          (M.mapKeys LocalDefinition (moDataTypes inputModule))
   in TCEnv
        { tceVars = getVarsInScope inputModule,
          tceDataTypes = dataTypes,
          tceInstances = builtInInstances <> instances,
          tceClasses = builtInClasses <> classes,
          tceConstraints = mempty
        }
