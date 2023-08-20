{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Smol.Core.Modules.ResolveDeps
  ( resolveModuleDeps,
    resolveExprDeps,
    resolveTypeclass
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
import Smol.Core.Modules.Types.DefIdentifier
import Smol.Core.Modules.Types.DepType
import Smol.Core.Modules.Types.Module
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Modules.Types.TopLevelExpression
import Smol.Core.Modules.Uses

resolveExprDeps ::
  (Show ann, MonadError ResolveDepsError m) =>
  Expr ParseDep ann ->
  Set Identifier ->
  Set (DefIdentifier ParseDep) ->
  m (Expr ResolvedDep ann)
resolveExprDeps expr typeclassMethods localDefs =
  evalStateT (resolveExpr expr typeclassMethods localDefs mempty) (ResolveState 0)

resolveExpr ::
  (Show ann, MonadError ResolveDepsError m, MonadState ResolveState m) =>
  Expr ParseDep ann ->
  Set Identifier ->
  Set (DefIdentifier ParseDep) ->
  Set Constructor ->
  m (Expr ResolvedDep ann)
resolveExpr expr typeclassMethods localDefs localTypes =
  runReaderT
    (resolveM expr)
    initialEnv
  where
    initialEnv = ResolveEnv mempty localDefs localTypes typeclassMethods

resolveModuleDeps ::
  (Show ann, Eq ann, MonadError ResolveDepsError m) =>
  Set Identifier ->
  Module ParseDep ann ->
  m (Module ResolvedDep ann, Map (DefIdentifier ResolvedDep) (Set (DefIdentifier ResolvedDep)))
resolveModuleDeps typeclassMethods parsedModule = do
  map' <- getDependencies extractUses parsedModule
  let resolveIt (DTData dt, _, _) =
        pure (DTData (resolveDataType dt))
      resolveIt (DTExpr expr, defIds, _entities) =
        DTExpr <$> resolveTopLevelExpression expr typeclassMethods defIds (allConstructors parsedModule)
      resolveIt (DTInstance inst, defIds, _entities) = do
        resolvedExpr <- resolveExpr (inExpr inst) typeclassMethods defIds (allConstructors parsedModule)
        pure (DTInstance (Instance {inConstraints = resolveConstraint <$> inConstraints inst,
            inExpr = resolvedExpr}))

  resolvedMap <- evalStateT (traverse resolveIt map') (ResolveState 0)

  let resolvedExpressions =
        mapMaybeWithKey
          ( \k a -> case (k, a) of
              (DIName identifier, DTExpr expr) -> Just (identifier, expr)
              _ -> Nothing
          )
          resolvedMap

      resolvedDataTypes =
        mapMaybeWithKey
          ( \k a -> case (k, a) of
              (DIType typeName, DTData dt) -> Just (typeName, dt)
              _ -> Nothing
          )
          resolvedMap

      resolvedInstances =
        mapMaybeWithKey
          ( \k a -> case (k, a) of
              (DIInstance constraint, DTInstance inst) -> Just (constraint, inst)
              _ -> Nothing
          )
          resolvedMap

      dependencies =
          M.fromList $
              (\(k,(_, b, _)) -> (resolveDefIdentifier k, S.map resolveDefIdentifier b)) <$>
                  M.toList map'

   in pure
        ( Module
            { moExpressions = resolvedExpressions,
              moDataTypes = resolvedDataTypes,
              moTests = moTests parsedModule,
              moInstances = M.mapKeys resolveConstraint resolvedInstances,
              moClasses = resolveTypeclass <$> moClasses parsedModule
            },
          dependencies
        )

mapMaybeWithKey :: (Ord k2) => (k -> a -> Maybe (k2, b)) -> Map k a -> Map k2 b
mapMaybeWithKey f = M.fromList . mapMaybe (uncurry f) . M.toList

allConstructors :: Module dep ann -> Set Constructor
allConstructors Module {moDataTypes} =
  foldMap (\(DataType {dtConstructors}) -> M.keysSet dtConstructors) moDataTypes

resolveDefIdentifier :: DefIdentifier ParseDep -> DefIdentifier ResolvedDep
resolveDefIdentifier (DIName name) = DIName name
resolveDefIdentifier (DITest test) = DITest test
resolveDefIdentifier (DIType ty) = DIType ty
resolveDefIdentifier (DIInstance inst) = DIInstance (resolveConstraint inst)

resolveTypeclass :: Typeclass ParseDep ann -> Typeclass ResolvedDep ann
resolveTypeclass (Typeclass {tcName,tcArgs,tcFuncName,tcFuncType} )
  = Typeclass {
    tcName, tcArgs, tcFuncName, tcFuncType = resolveType tcFuncType
              }

resolveDataType :: DataType ParseDep ann -> DataType ResolvedDep ann
resolveDataType (DataType {dtName, dtVars, dtConstructors}) =
  DataType dtName dtVars (resolveDataConstructor <$> dtConstructors)
  where
    resolveDataConstructor tys =
      resolveType <$> tys

resolveConstraint :: Constraint ParseDep ann -> Constraint ResolvedDep ann
resolveConstraint (Constraint tcn tys)
  = Constraint tcn (resolveType <$> tys)

resolveType :: Type ParseDep ann -> Type ResolvedDep ann
resolveType (TVar ann (ParseDep v _)) = TVar ann (LocalDefinition v)
resolveType (TConstructor ann c) = TConstructor ann (resolveTypeName c)
resolveType (TInfix ann op a b) = TInfix ann op (resolveType a) (resolveType b)
resolveType (TPrim ann p) = TPrim ann p
resolveType (TLiteral ann l) = TLiteral ann l
resolveType (TFunc ann closure from to) =
  TFunc ann (M.mapKeys resolveId $ resolveType <$> closure) (resolveType from) (resolveType to)
  where
    resolveId (ParseDep v _) = LocalDefinition v
resolveType (TTuple ann a as) =
  TTuple ann (resolveType a) (resolveType <$> as)
resolveType (TArray ann size a) = TArray ann size (resolveType a)
resolveType (TUnknown ann i) = TUnknown ann i
resolveType (TRecord ann as) = TRecord ann (resolveType <$> as)
resolveType (TApp ann fn arg) = TApp ann (resolveType fn) (resolveType arg)

-- resolve Expr (s) and Type pls
resolveTopLevelExpression ::
  (Show ann, MonadState ResolveState m, MonadError ResolveDepsError m) =>
  TopLevelExpression ParseDep ann ->
  Set Identifier ->
  Set (DefIdentifier ParseDep) ->
  Set Constructor ->
  m (TopLevelExpression ResolvedDep ann)
resolveTopLevelExpression tle typeclassMethods localDefs localTypes = flip runReaderT initialEnv $ do
  resolvedExpr <- resolveM (tleExpr tle)
  let resolvedType = fmap resolveType (tleType tle)

  pure
    ( TopLevelExpression
        { tleConstraints = resolveConstraint <$> tleConstraints tle,
          tleExpr = resolvedExpr,
          tleType = resolvedType
        }
    )
  where
    initialEnv = ResolveEnv mempty localDefs localTypes typeclassMethods

resolveIdentifier ::
  ( MonadReader ResolveEnv m,
    MonadState ResolveState m,
    MonadError ResolveDepsError m
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
        else do
          isTypeclassMethod <- asks (S.member ident . reTypeclassMethods)
          if isTypeclassMethod
            then typeclassIdentifier ident
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

typeclassIdentifier ::
  (MonadState ResolveState m) =>
  Identifier ->
  m (ResolvedDep Identifier)
typeclassIdentifier ident =
  TypeclassCall ident <$> freshInt

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
    reLocal :: Set (DefIdentifier ParseDep),
    reLocalConstructor :: Set Constructor,
    reTypeclassMethods :: Set Identifier
  }
  deriving stock (Eq, Ord, Show)

newtype ResolveState = ResolveState {rsUnique :: Int}

resolveM ::
  (Show ann, MonadReader ResolveEnv m, MonadState ResolveState m, MonadError ResolveDepsError m) =>
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
    MonadError ResolveDepsError m,
    MonadState ResolveState m
  ) =>
  Pattern ParseDep ann ->
  m (Pattern ResolvedDep ann, Map Identifier Int)
resolvePattern = runWriterT . resolvePatternInner
  where
    resolvePatternInner ::
      ( MonadError ResolveDepsError m,
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
