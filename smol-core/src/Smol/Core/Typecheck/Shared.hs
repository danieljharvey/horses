{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Typecheck.Shared
  ( getExprAnnotation,
    getPatternAnnotation,
    getTypeAnnotation,
    getUnknown,
    getClosureType,
    reduceType,
    lookupVar,
    popArg,
    popArgs,
    flattenConstructorType,
    flattenConstructorApplication,
    withVar,
    dataTypeWithVars,
    withNewVars,
    pushArg,
    getApplyReturnType,
    withGlobal,
    lookupGlobal,
    lookupConstructor,
    lookupTypeName,
    typeForConstructor,
    tellGlobal,
    listenGlobals,
    freshen,
    primFromTypeLiteral,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Smol.Core.Helpers
import Smol.Core.Typecheck.FreeVars
import Smol.Core.Typecheck.Substitute
import Smol.Core.Typecheck.Types
import Smol.Core.Types
import Smol.Core.Types.Expr
import Smol.Core.Types.ResolvedDep

lookupTypeName ::
  ( MonadReader (TCEnv ann) m
  ) =>
  TypeName ->
  m (DataType ann)
lookupTypeName tn = do
  maybeDt <- asks (M.lookup tn . tceDataTypes)
  case maybeDt of
    Just dt -> pure dt
    Nothing -> error $ "couldn't find datatype for " <> show tn

getExprAnnotation :: Expr dep ann -> ann
getExprAnnotation (EInfix ann _ _ _) = ann
getExprAnnotation (EConstructor ann _) = ann
getExprAnnotation (ELet ann _ _ _) = ann
getExprAnnotation (ELambda ann _ _) = ann
getExprAnnotation (EPrim ann _) = ann
getExprAnnotation (EApp ann _ _) = ann
getExprAnnotation (EIf ann _ _ _) = ann
getExprAnnotation (EAnn ann _ _) = ann
getExprAnnotation (EVar ann _) = ann
getExprAnnotation (ETuple ann _ _) = ann
getExprAnnotation (EGlobal ann _) = ann
getExprAnnotation (EGlobalLet ann _ _ _) = ann
getExprAnnotation (ERecord ann _) = ann
getExprAnnotation (ERecordAccess ann _ _) = ann
getExprAnnotation (EPatternMatch ann _ _) = ann

getPatternAnnotation :: Pattern dep ann -> ann
getPatternAnnotation (PVar ann _) = ann
getPatternAnnotation (PWildcard ann) = ann
getPatternAnnotation (PTuple ann _ _) = ann
getPatternAnnotation (PLiteral ann _) = ann
getPatternAnnotation (PConstructor ann _ _) = ann

getTypeAnnotation :: Type ann -> ann
getTypeAnnotation (TPrim ann _) = ann
getTypeAnnotation (TUnknown ann _) = ann
getTypeAnnotation (TConstructor ann _) = ann
getTypeAnnotation (TApp ann _ _) = ann
getTypeAnnotation (TFunc ann _ _ _) = ann
getTypeAnnotation (TTuple ann _ _) = ann
getTypeAnnotation (TVar ann _) = ann
getTypeAnnotation (TGlobals ann _ _) = ann
getTypeAnnotation (TLiteral ann _) = ann
getTypeAnnotation (TRecord ann _) = ann
getTypeAnnotation (TUnion ann _ _) = ann

primFromTypeLiteral :: TypeLiteral -> Prim
primFromTypeLiteral (TLInt i) = PInt i
primFromTypeLiteral (TLBool b) = PBool b
primFromTypeLiteral TLUnit = PUnit

getUnknown :: (MonadState (TCState ann) m) => ann -> m (Type ann)
getUnknown ann = do
  count <- gets tcsUnknown
  modify (\s -> s {tcsUnknown = count + 1})
  pure (TUnknown ann count)

-- | this needs some thought. Our closures shouldn't contain any external deps,
-- but we do want to ensure uniqueness, so our Identifier type should probably
-- be `(Identifier, Maybe Int)` or something
getClosureType ::
  ( MonadState (TCState ann) m,
    MonadReader (TCEnv ann) m,
    MonadError (TCError ann) m,
    Ord ann
  ) =>
  ResolvedExpr (Type ann) ->
  m (Map Identifier (Type ann))
getClosureType body =
  mconcat
    <$> traverse
      ( \ident ->
          M.singleton (rdIdentifier ident) <$> lookupVar ident
      )
      (S.toList (freeVars body))

-- reduce TApp (TFunc a b) etc
reduceType :: Type ann -> Type ann
reduceType = reduceTypeInner
  where
    reduceTypeInner (TApp ann (TApp _ (TFunc _ _ (TVar _ varA) body) a) b) =
      reduceTypeInner (TApp ann (substituteMany [Substitution (SubId varA) a] body) b)
    reduceTypeInner (TApp _ (TFunc _ _ (TVar _ var) body) a) =
      reduceTypeInner (substituteMany [Substitution (SubId var) a] body)
    reduceTypeInner (TApp ann (TApp _ (TFunc _ _ (TUnknown _ iA) body) a) b) =
      reduceTypeInner (TApp ann (substituteMany [Substitution (SubUnknown iA) a] body) b)
    reduceTypeInner (TApp _ (TFunc _ _ (TUnknown _ iB) body) a) =
      reduceTypeInner (substituteMany [Substitution (SubUnknown iB) a] body)
    reduceTypeInner other = other

getApplyReturnType ::
  (MonadError (TCError ann) m) =>
  Type ann ->
  m (Type ann)
getApplyReturnType (TFunc _ _ _ typ) = pure typ
getApplyReturnType tApp@TApp {} = pure tApp
getApplyReturnType other =
  throwError (TCExpectedFunction other)

-- | given the constructor name, see where it lives and gather details
lookupConstructor ::
  ( MonadReader (TCEnv ann) m,
    MonadError (TCError ann) m
  ) =>
  ResolvedDep Constructor ->
  m (TypeName, [Identifier], [Constructor], [Type ann])
lookupConstructor constructor = do
  maybeDt <-
    asks
      ( mapFind
          ( \(DataType typeName vars constructors) ->
              (,,,) typeName vars (M.keys constructors) <$> M.lookup (rdIdentifier constructor) constructors
          )
          . tceDataTypes
      )
  case maybeDt of
    Just dtInfo -> pure dtInfo
    Nothing -> do
      allDataTypes <- asks tceDataTypes
      let availableConstructors = concatMap (\(DataType _ _ as) -> M.keys as) (M.elems allDataTypes)
      throwError (TCUnknownConstructor constructor availableConstructors)

dataTypeWithVars ::
  ann ->
  TypeName ->
  [Type ann] ->
  Type ann
dataTypeWithVars ann tyName =
  foldl'
    (TApp ann)
    (TConstructor ann tyName)

typeForConstructor ::
  (MonadState (TCState ann) m) =>
  ann ->
  TypeName ->
  [Identifier] ->
  [Type ann] ->
  m (Type ann)
typeForConstructor ann typeName vars args = do
  -- replace variables with fresh boys
  subs <- traverse (\var -> Substitution (SubId var) <$> getUnknown ann) vars

  pure $
    substituteMany subs $
      foldr
        (TFunc ann mempty)
        (dataTypeWithVars ann typeName (TVar ann <$> vars))
        args

lookupVar ::
  (MonadReader (TCEnv ann) m, MonadError (TCError ann) m) =>
  ResolvedDep Identifier ->
  m (Type ann)
lookupVar ident = do
  maybeVar <- asks (M.lookup ident . tceVars)
  case maybeVar of
    Just expr -> pure expr
    Nothing -> throwError (TCCouldNotFindVar ident)

withVar ::
  (MonadReader (TCEnv ann) m) =>
  ResolvedDep Identifier ->
  Type ann ->
  m a ->
  m a
withVar ident expr =
  local
    ( \env ->
        env {tceVars = M.singleton ident expr <> tceVars env}
    )

withGlobal ::
  (MonadReader (TCEnv ann) m) =>
  Identifier ->
  Type ann ->
  m a ->
  m a
withGlobal ident expr =
  local
    ( \env ->
        env {tceGlobals = M.singleton ident expr <> tceGlobals env}
    )

lookupGlobal ::
  (MonadReader (TCEnv ann) m, MonadError (TCError ann) m) =>
  Identifier ->
  m (Type ann)
lookupGlobal ident = do
  maybeVar <- asks (M.lookup ident . tceGlobals)
  case maybeVar of
    Just expr -> pure expr
    Nothing -> throwError (TCCouldNotFindGlobal ident)

pushArg ::
  (MonadState (TCState ann) m) =>
  Type ann ->
  m ()
pushArg typ = do
  modify
    ( \st ->
        st {tcsArgStack = typ : tcsArgStack st}
    )

-- | pass stack arg to action and remove it
popArg :: (MonadState (TCState ann) m) => m (Maybe (Type ann))
popArg = do
  topVal <- gets (listToMaybe . tcsArgStack)
  modify
    ( \st ->
        st
          { tcsArgStack = case tcsArgStack st of
              [] -> []
              as -> tail as
          }
    )
  pure topVal

-- | replace TVar with new TUnknown
freshen :: (MonadState (TCState ann) m) => Type ann -> m (Type ann)
freshen ty = do
  subs <-
    traverse
      ( \var -> do
          unknown <- getUnknown (getTypeAnnotation ty)
          pure $ Substitution (SubId var) unknown
      )
      (S.toList $ freeTypeVars ty)
  pure $ substituteMany subs ty

-- | pass stack arg to action and remove it
popArgs :: (Show ann, MonadState (TCState ann) m) => Int -> m [Type ann]
popArgs 0 = pure mempty
popArgs maxArgs = do
  maybeArg <- popArg
  case maybeArg of
    Just arg -> do
      moreArgs <- popArgs (maxArgs - 1)
      pure $ arg : moreArgs
    Nothing -> pure mempty

-- untangle a bunch of TApp (TApp (TConstructor typeName) 1) True into `(typeName, [1, True])`
-- to make it easier to match up with patterns
flattenConstructorType ::
  (MonadError (TCError ann) m) =>
  Type ann ->
  m (TypeName, [Type ann])
flattenConstructorType (TApp _ f a) = do
  (typeName, as) <- flattenConstructorType f
  pure (typeName, as <> [a])
flattenConstructorType (TConstructor _ typeName) =
  pure (typeName, mempty)
flattenConstructorType ty = throwError (TCExpectedConstructorType ty)

-- untangle a bunch of TApp (TApp (TConstructor typeName) 1) True into `(typeName, [1, True])`
-- to make it easier to match up with patterns
flattenConstructorApplication ::
  Expr dep ann -> Maybe (dep Constructor, [Expr dep ann])
flattenConstructorApplication (EApp _ f a) = do
  (constructor, as) <- flattenConstructorApplication f
  pure (constructor, as <> [a])
flattenConstructorApplication (EConstructor _ constructor) =
  pure (constructor, mempty)
flattenConstructorApplication _ = Nothing

-- given a map of identifiers to types, run the enclosed action
-- useful for typechecking the right hand side of a pattern match, where
-- we add in the bound variables (but they don't exist outside this context)
withNewVars ::
  (MonadReader (TCEnv ann) m) =>
  Map (ResolvedDep Identifier) (Type ann) ->
  m a ->
  m a
withNewVars vars =
  local (\env -> env {tceVars = vars <> tceVars env})

tellGlobal ::
  (MonadState (TCState ann) m) =>
  GlobalMap ann ->
  m ()
tellGlobal globs =
  modify (\s -> s {tcsGlobals = tcsGlobals s <> [globs]})

listenGlobals ::
  ( Eq ann,
    MonadState (TCState ann) m
  ) =>
  m a ->
  m (a, [GlobalMap ann])
listenGlobals action = do
  stateBefore <- gets tcsGlobals
  a <- action
  stateAfter <- gets tcsGlobals
  let newState = filter (`notElem` stateBefore) stateAfter
  modify
    ( \s ->
        s
          { tcsGlobals =
              filter
                (`notElem` newState)
                (tcsGlobals s)
          }
    )
  pure (a, newState)
