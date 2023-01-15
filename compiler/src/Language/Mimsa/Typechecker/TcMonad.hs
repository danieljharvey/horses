{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Typechecker.TcMonad
  ( defaultTcState,
    getUnknown,
    instantiate,
    addTypedHole,
    getTypedHoles,
    TypecheckState (..),
    variableToTypeIdentifier,
    getNextUniVar,
  )
where

import Control.Monad.Except
import Control.Monad.State (MonadState, gets, modify)
import Data.Coerce
import Data.Functor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Language.Mimsa.Core
import Language.Mimsa.Typechecker.Generalise
import Language.Mimsa.Types.Error.TypeError
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Types.Typechecker.Substitutions
import Language.Mimsa.Types.Typechecker.Unique

data TypecheckState = TypecheckState
  { tcsNum :: Int,
    tcsTypedHoles :: Map Name (Annotation, Int, Map Name MonoType)
  }

instantiate ::
  (MonadState TypecheckState m) => Annotation -> Scheme -> m MonoType
instantiate ann (Scheme vars ty) = do
  newVars <- traverse (const (getUnknown ann)) vars
  let pairs = zip vars newVars
  let subst = Substitutions $ M.fromList pairs
  let substitutedType = applySubst subst ty
  pure (substitutedType $> ann) -- use original annotation

-- | get starting typechecker state,
-- make sure our fresh vars number is higher than any we've seen before
defaultTcState :: Environment -> TypecheckState
defaultTcState env =
  let maxInTypeMap = case getUniVar <$> freeTypeVarsCtx env of
        [] -> 0
        as -> maximum (catMaybes as)
   in TypecheckState (maxInTypeMap + 1) mempty

getNextUniVar :: (MonadState TypecheckState m) => m Int
getNextUniVar = do
  nextUniVar <- gets tcsNum
  modify (\s -> s {tcsNum = nextUniVar + 1})
  pure nextUniVar

typeFromUniVar :: Annotation -> Int -> MonoType
typeFromUniVar ann = MTVar ann . TVUnificationVar

getUnknown :: (MonadState TypecheckState m) => Annotation -> m MonoType
getUnknown ann =
  typeFromUniVar ann
    <$> getNextUniVar

-- | Get a new unknown for a typed hole and return it's monotype
addTypedHole ::
  ( MonadState TypecheckState m,
    MonadError TypeError m
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
  pure $ MTVar ann (TVUnificationVar i)

-- capture type schemes currently in scope
-- instantiate them now
schemesToTypeMap ::
  (MonadState TypecheckState m, MonadError TypeError m) =>
  Map TypeIdentifier Scheme ->
  m (Map Name MonoType)
schemesToTypeMap schemes = do
  let fn (k, v) =
        let leName = case k of
              TVName n -> pure (Name $ coerce n)
              TVUnificationVar _i ->
                throwError UnknownTypeError -- TODO: bespoke error
              TVScopedVar _ name -> pure name
         in (,) <$> leName <*> instantiate mempty v
  typeMap <- traverse fn (M.toList schemes)
  pure (M.fromList typeMap)

getTypedHoles ::
  (MonadState TypecheckState m) =>
  Substitutions ->
  m (Map Name (MonoType, Map Name MonoType))
getTypedHoles subs'@(Substitutions subs) = do
  holes <- gets tcsTypedHoles
  let getMonoType _ (ann, i, localTypeMap) =
        case M.lookup (TVUnificationVar i) subs of
          Just a ->
            ( applySubst subs' a,
              applySubst subs' localTypeMap
            )
          Nothing ->
            ( applySubst subs' (MTVar ann (TVUnificationVar i)),
              applySubst subs' localTypeMap
            )
  pure $ M.mapWithKey getMonoType holes

variableToTypeIdentifier :: (Name, Unique hash) -> TypeIdentifier
variableToTypeIdentifier (name, Unique i) = TVScopedVar i name
variableToTypeIdentifier (name, _) =
  TVName (coerce name)
