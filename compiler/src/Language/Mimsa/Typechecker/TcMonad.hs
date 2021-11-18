{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.TcMonad
  ( defaultTcState,
    getUnknown,
    instantiate,
    addTypedHole,
    getTypedHoles,
    TypecheckState (..),
    variableToTypeIdentifier,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (MonadState, gets, modify)
import Data.Coerce
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

data TypecheckState = TypecheckState
  { tcsNum :: Int,
    tcsTypedHoles :: Map Name (Annotation, Int, Map Name MonoType),
    tcsTypeStack :: [MonoType]
  }

instantiate ::
  (MonadState TypecheckState m) => Annotation -> Scheme -> m MonoType
instantiate ann (Scheme vars ty) = do
  newVars <- traverse (const $ getUnknown ann) vars
  let pairs = zip vars newVars
  let subst = Substitutions $ M.fromList pairs
  let substitutedType = applySubst subst ty
  pure (substitutedType $> ann) -- use original annotation

defaultTcState :: TypecheckState
defaultTcState = TypecheckState 0 mempty mempty

getNextUniVar :: (MonadState TypecheckState m) => m Int
getNextUniVar = do
  nextUniVar <- gets tcsNum
  modify (\s -> s {tcsNum = nextUniVar + 1})
  pure nextUniVar

typeFromUniVar :: Annotation -> Int -> MonoType
typeFromUniVar ann = MTVar ann . TVNum

getUnknown :: (MonadState TypecheckState m) => Annotation -> m MonoType
getUnknown ann = typeFromUniVar ann <$> getNextUniVar

-- | Get a new unknown for a typed hole and return it's monotype
addTypedHole ::
  ( MonadState TypecheckState m,
    MonadError TypeError m,
    MonadReader Swaps m
  ) =>
  Environment ->
  Annotation ->
  Name ->
  m MonoType
addTypedHole env ann name = do
  i <- getNextUniVar
  localTypeMap <- schemesToTypeMap (getSchemes env)
  modify
    ( \s ->
        s
          { tcsTypedHoles =
              tcsTypedHoles s
                <> M.singleton name (ann, i, localTypeMap)
          }
    )
  pure $ MTVar ann (TVNum i)

-- | if we have a `Variable`, lookup it's original `Name` in Reader context
lookupSwap ::
  ( MonadReader Swaps m,
    MonadError TypeError m
  ) =>
  Variable ->
  m Name
lookupSwap var = do
  swaps <- ask
  case M.lookup var swaps of
    Just a -> pure a
    _ -> throwError UnknownTypeError -- forgive me, Padre

-- capture type schemes currently in scope
-- instantiate them now
schemesToTypeMap ::
  (MonadState TypecheckState m, MonadError TypeError m, MonadReader Swaps m) =>
  Map TypeIdentifier Scheme ->
  m (Map Name MonoType)
schemesToTypeMap schemes = do
  let fn (k, v) =
        let leName = case k of
              TVName n -> pure (Name $ coerce n)
              TVNum i -> lookupSwap (NumberedVar i)
         in (,) <$> leName <*> instantiate mempty v

  typeMap <- traverse fn (M.toList schemes)
  pure (M.fromList typeMap)

-- todo - look up index in substitutions to get type
getTypedHoles ::
  (MonadState TypecheckState m) =>
  Substitutions ->
  m (Map Name (MonoType, Map Name MonoType))
getTypedHoles subs'@(Substitutions subs) = do
  holes <- gets tcsTypedHoles
  let getMonoType (ann, i, localTypeMap) = case M.lookup (TVNum i) subs of
        Just a -> (applySubst subs' a, applySubst subs' localTypeMap)
        Nothing -> (applySubst subs' (MTVar ann (TVNum i)), applySubst subs' localTypeMap)
  pure $ fmap getMonoType holes

variableToTypeIdentifier :: Variable -> TypeIdentifier
variableToTypeIdentifier (NamedVar n) = TVName (coerce n)
variableToTypeIdentifier (NumberedVar i) = TVNum i
