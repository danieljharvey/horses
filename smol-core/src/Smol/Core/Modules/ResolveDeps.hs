{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Modules.ResolveDeps
  ( resolveModuleDeps,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Smol.Core
import Smol.Core.Modules.Dependencies
import Smol.Core.Modules.Uses
import Smol.Core.Types.Module.DefIdentifier
import Smol.Core.Types.Module.Module

-- this is possibly only useful for testing
resolveModuleDeps :: (Eq ann) => Module ParseDep ann -> Module ResolvedDep ann
resolveModuleDeps parsedModule =
      case getDependencies extractUses parseModule of
        Right map' ->
          -- now we need to resolve each thing one by one
          -- then remake the module with the shiny new shit in it
          let resolveIt (DTData {},_,_) = error "not thinking about data yet"
              resolveIt (DTExpr expr, defIds, _entities) =
                resolve newExpr defIds
            _ -> error "did not find my boy"
        Left e -> error (show e)

resolve ::
  Expr ParseDep ann ->
  Set DefIdentifier ->
  Expr ResolvedDep ann
resolve expr _localDefs =
  runReader
    (evalStateT (resolveM expr) initialState)
    initialEnv
  where
    initialEnv = ResolveEnv mempty
    initialState = ResolveState 0

resolveIdentifier ::
  ( MonadReader ResolveEnv m
  ) =>
  ParseDep Identifier ->
  m (ResolvedDep Identifier)
resolveIdentifier (ParseDep ident (Just modName)) =
  error $ "could not resolve " <> show ident <> " in module " <> show modName
resolveIdentifier (ParseDep ident Nothing) = do
  existingUnique <- asks (M.lookup ident . reExisting)
  case existingUnique of
    Just i -> pure (UniqueDefinition ident i)
    Nothing -> error "dont know how to make a new unique yet"

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

newtype ResolveEnv = ResolveEnv {reExisting :: Map Identifier Int}

newtype ResolveState = ResolveState {rsUnique :: Int}

resolveM ::
  (MonadReader ResolveEnv m, MonadState ResolveState m) =>
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
resolveM _ = error "resolveM"
