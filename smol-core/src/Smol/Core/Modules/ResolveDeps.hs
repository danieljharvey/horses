{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Smol.Core.Modules.ResolveDeps
  ( resolveModuleDeps,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Smol.Core
import Smol.Core.Modules.Dependencies
import Smol.Core.Modules.ModuleError
import Smol.Core.Modules.Types.DepType
import Smol.Core.Modules.Uses
import Smol.Core.Types.Module.DefIdentifier
import Smol.Core.Types.Module.Module

resolveModuleDeps ::
  (Show ann, Eq ann, MonadError ModuleError m) =>
  Module ParseDep ann ->
  m (Module ResolvedDep ann, Map DefIdentifier (Set DefIdentifier))
resolveModuleDeps parsedModule = do
  map' <- getDependencies extractUses parsedModule
  let resolveIt (DTData dt, _, _) =
        pure (Left (resolveDataType dt))
      resolveIt (DTExpr expr, defIds, _entities) =
        Right <$> resolveExpr expr defIds (allConstructors parsedModule)
  resolvedMap <- traverse resolveIt map'
  let newExpressions =
        M.mapMaybe
          ( \case
              Right expr -> Just expr
              Left _ -> Nothing
          )
          resolvedMap
      newDataTypes =
        mapMaybeWithKey
          ( \k a -> case (k, a) of
              (DIType typeName, Left dt) -> Just (typeName, dt)
              _ -> Nothing
          )
          resolvedMap
      dependencies = (\(_, b, _) -> b) <$> map'
   in pure
        ( parsedModule
            { moExpressions = newExpressions,
              moDataTypes = newDataTypes
            },
          dependencies
        )

mapMaybeWithKey :: (Ord k2) => (k -> a -> Maybe (k2, b)) -> Map k a -> Map k2 b
mapMaybeWithKey f = M.fromList . mapMaybe (uncurry f) . M.toList

allConstructors :: Module dep ann -> Set Constructor
allConstructors Module {moDataTypes} =
  foldMap (\(DataType {dtConstructors}) -> M.keysSet dtConstructors) moDataTypes

resolveDataType :: DataType ParseDep ann -> DataType ResolvedDep ann
resolveDataType (DataType {dtName, dtVars, dtConstructors}) =
  DataType dtName dtVars (resolveDataConstructor <$> dtConstructors)
  where
    resolveDataConstructor tys =
      resolveType <$> tys

resolveType :: Type ParseDep ann -> Type ResolvedDep ann
resolveType (TVar ann (ParseDep v _)) = TVar ann (LocalDefinition v)
resolveType (TConstructor ann c) = TConstructor ann (resolveTypeName c)
resolveType (TPrim ann p) = TPrim ann p
resolveType (TLiteral ann l) = TLiteral ann l
resolveType (TFunc ann closure from to) =
  TFunc ann (resolveType <$> closure) (resolveType from) (resolveType to)
resolveType (TTuple ann a as) =
  TTuple ann (resolveType a) (resolveType <$> as)
resolveType (TArray ann size a) = TArray ann size (resolveType a)
resolveType (TUnknown ann i) = TUnknown ann i
resolveType (TGlobals ann bits inner) =
  TGlobals ann (resolveType <$> bits) (resolveType inner)
resolveType (TRecord ann as) = TRecord ann (resolveType <$> as)
resolveType (TApp ann fn arg) = TApp ann (resolveType fn) (resolveType arg)

resolveExpr ::
  (Show ann, MonadError ModuleError m) =>
  Expr ParseDep ann ->
  Set DefIdentifier ->
  Set Constructor ->
  m (Expr ResolvedDep ann)
resolveExpr expr localDefs localTypes =
  liftEither $
    runReader
      (evalStateT (runExceptT $ resolveM expr) initialState)
      initialEnv
  where
    initialEnv = ResolveEnv mempty localDefs localTypes
    initialState = ResolveState 0

resolveIdentifier ::
  ( MonadReader ResolveEnv m,
    MonadError ModuleError m
  ) =>
  ParseDep Identifier ->
  m (ResolvedDep Identifier)
resolveIdentifier (ParseDep ident (Just modName)) =
  error $ "could not resolve " <> show ident <> " in module " <> show modName
resolveIdentifier (ParseDep ident Nothing) = do
  existingUnique <- asks (M.lookup ident . reExisting)
  case existingUnique of
    Just i -> pure (UniqueDefinition ident i)
    Nothing -> do
      isLocal <- asks (S.member (DIName ident) . reLocal)
      if isLocal
        then pure (LocalDefinition ident)
        else throwError $ VarNotFound ident

resolveConstructor ::
  ( MonadReader ResolveEnv m
  ) =>
  ParseDep Constructor ->
  m (ResolvedDep Constructor)
resolveConstructor (ParseDep constructor (Just modName)) =
  error $ "could not resolve " <> show constructor <> " in module " <> show modName
resolveConstructor (ParseDep constructor Nothing) = do
  isLocal <- asks (S.member constructor . reLocalConstructor)
  if isLocal
    then pure (LocalDefinition constructor)
    else error $ "Could not find constructor " <> show constructor

resolveTypeName ::
  ParseDep TypeName ->
  ResolvedDep TypeName
resolveTypeName (ParseDep tn Nothing) =
  LocalDefinition tn
resolveTypeName (ParseDep tn (Just a)) =
  error $ "resolve type name for type " <> show a <> "." <> show tn

freshInt :: (MonadState ResolveState m) => m Int
freshInt =
  state
    ( \rs ->
        let newInt = rsUnique rs + 1
         in (newInt, rs {rsUnique = newInt})
    )

newIdentifier ::
  (MonadState ResolveState m) =>
  ParseDep Identifier ->
  m (Int, Identifier, ResolvedDep Identifier)
newIdentifier (ParseDep ident _) = do
  i <- freshInt
  pure (i, ident, UniqueDefinition ident i)

withNewIdentifier ::
  (MonadReader ResolveEnv m) =>
  Int ->
  Identifier ->
  m a ->
  m a
withNewIdentifier i ident =
  local (\re -> re {reExisting = M.singleton ident i <> reExisting re})

withNewIdentifiers ::
  (MonadReader ResolveEnv m) =>
  Map Identifier Int ->
  m a ->
  m a
withNewIdentifiers resolvedIdentifiers =
  local (\re -> re {reExisting = resolvedIdentifiers <> reExisting re})

data ResolveEnv = ResolveEnv
  { reExisting :: Map Identifier Int,
    reLocal :: Set DefIdentifier,
    reLocalConstructor :: Set Constructor
  }

newtype ResolveState = ResolveState {rsUnique :: Int}

resolveM ::
  (Show ann, MonadReader ResolveEnv m, MonadState ResolveState m, MonadError ModuleError m) =>
  Expr ParseDep ann ->
  m (Expr ResolvedDep ann)
resolveM (EVar ann ident) = EVar ann <$> resolveIdentifier ident
resolveM (ELet ann ident body rest) = do
  (unique, innerIdent, newIdent) <- newIdentifier ident
  (body', rest') <-
    withNewIdentifier
      unique
      innerIdent
      ((,) <$> resolveM body <*> resolveM rest)
  pure (ELet ann newIdent body' rest')
resolveM (EPrim ann prim) = pure (EPrim ann prim)
resolveM (EApp ann fn arg) =
  EApp ann <$> resolveM fn <*> resolveM arg
resolveM (EConstructor ann constructor) =
  EConstructor ann <$> resolveConstructor constructor
resolveM (ELambda ann ident body) = do
  (unique, innerIdent, newIdent) <- newIdentifier ident
  resolvedBody <- withNewIdentifier unique innerIdent (resolveM body)
  pure $ ELambda ann newIdent resolvedBody
resolveM (EInfix ann op a b) =
  EInfix ann op <$> resolveM a <*> resolveM b
resolveM (EIf ann predExpr thenExpr elseExpr) =
  EIf ann
    <$> resolveM predExpr
    <*> resolveM thenExpr
    <*> resolveM elseExpr
resolveM (EAnn ann ty expr) =
  EAnn ann (resolveType ty) <$> resolveM expr
resolveM (ETuple ann a as) =
  ETuple ann <$> resolveM a <*> traverse resolveM as
resolveM (EArray ann as) =
  EArray ann <$> traverse resolveM as
resolveM (EGlobal ann g) =
  pure $ EGlobal ann g
resolveM (EGlobalLet ann g expr rest) =
  EGlobalLet ann g <$> resolveM expr <*> resolveM rest
resolveM (ERecord ann as) =
  ERecord ann <$> traverse resolveM as
resolveM (ERecordAccess ann expr name) =
  ERecordAccess ann <$> resolveM expr <*> pure name
resolveM (EPatternMatch ann expr pats) = do
  EPatternMatch ann <$> resolveM expr <*> traverse (uncurry resolvePat) pats
  where
    resolvePat pat patExpr = do
      (resolvedPat, idents) <- resolvePattern pat
      (,) resolvedPat <$> withNewIdentifiers idents (resolveM patExpr)

resolvePattern ::
  forall m ann.
  ( MonadReader ResolveEnv m,
    MonadError ModuleError m,
    MonadState ResolveState m
  ) =>
  Pattern ParseDep ann ->
  m (Pattern ResolvedDep ann, Map Identifier Int)
resolvePattern = runWriterT . resolvePatternInner
  where
    resolvePatternInner ::
      ( MonadError ModuleError m,
        MonadReader ResolveEnv m,
        MonadWriter (Map Identifier Int) m,
        MonadState ResolveState m
      ) =>
      Pattern ParseDep ann ->
      m (Pattern ResolvedDep ann)
    resolvePatternInner (PVar ann ident) = do
      (unique, innerIdent, newIdent) <- newIdentifier ident
      tell (M.singleton innerIdent unique)
      pure (PVar ann newIdent)
    resolvePatternInner (PWildcard ann) = pure (PWildcard ann)
    resolvePatternInner (PTuple ann a as) =
      PTuple ann <$> resolvePatternInner a <*> traverse resolvePatternInner as
    resolvePatternInner (PArray ann as spread) =
      PArray ann
        <$> traverse resolvePatternInner as
        <*> case spread of
          NoSpread -> pure NoSpread
          SpreadWildcard ann' -> pure (SpreadWildcard ann')
          SpreadValue ann' v -> SpreadValue ann' <$> resolveIdentifier v
    resolvePatternInner (PLiteral ann l) =
      pure $ PLiteral ann l
    resolvePatternInner (PConstructor ann constructor args) =
      PConstructor ann
        <$> resolveConstructor constructor
        <*> traverse resolvePatternInner args
