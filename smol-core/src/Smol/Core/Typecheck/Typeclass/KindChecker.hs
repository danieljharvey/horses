{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
  {-# LANGUAGE NamedFieldPuns #-}
  module Smol.Core.Typecheck.Typeclass.KindChecker (Kind (..), UKind (..), unifyKinds, typeKind, lookupKindInType) where

import Data.Maybe (listToMaybe)
import Debug.Trace
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import Data.Functor
import qualified Data.Map as M
import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Core.TypeUtils (monoidType)
import Smol.Core.Typecheck.Annotations
import Smol.Core.Types.DataType
import Smol.Core.Types.Identifier
import Smol.Core.Types.Type
import Smol.Core.Types.TypeName

data Kind
  = Star
  | KindFn Kind Kind
  deriving stock (Eq, Ord, Show)

-- | Unresolved Kind
data UKind i
  = UStar
  | UKindFn (UKind i) (UKind i)
  | UVar i
  deriving stock (Eq, Ord, Show)

instance (Show i) => Printer (UKind i) where
  prettyDoc = PP.pretty . show

data KindState dep ann = KindState
  { ksDataTypes :: M.Map (dep TypeName) (DataType dep ann),
    ksInt :: Int,
    ksEnv :: [(UKind Int, UKind Int)] -- unique, what it is
  }

data KindError i = KindMismatch (UKind i) (UKind i) 
 | UnassignedVar i
  deriving stock (Eq,Ord,Show)

typeKind ::
  (MonadError (KindError Int) m, Ord (dep TypeName), Show (dep TypeName), Show (dep Identifier)) =>
  M.Map (dep TypeName) (DataType dep ann) ->
  Type dep ann ->
  m (Type dep Kind)
typeKind dts ty = do 
  let (ty', ks) = flip runState (KindState dts 1 mempty) (inferKinds ty)
  subs <- solve (ksEnv ks) 
  unifyType subs (traceShowId ty') 

-- given a bunch of substitutions
-- run them all
-- then create one more, which is the resulting kind equalling star
unifyType :: (MonadError (KindError Int) m) =>
    M.Map Int (UKind Int) -> Type dep (UKind Int) -> m (Type dep Kind)
unifyType subs ty =
  let applyToKind k = do 
        let appliedKind = applySubstitutions subs k
        let newConstraints = [(appliedKind, UStar)] -- ie, every type should have kind Star
        newSubs <- solve newConstraints
        toKind (applySubstitutions newSubs appliedKind) 
  in traverse applyToKind ty

toKind :: (MonadError (KindError i) m, Show i) => UKind i -> m Kind
toKind UStar = pure $ Star
toKind (UKindFn a b) = KindFn <$> toKind a <*> toKind b
toKind (UVar i) = throwError (UnassignedVar i) 

applySubstitution :: (Eq i) => (i, UKind i) -> UKind i -> UKind i
applySubstitution (i, sub) (UVar i') | i == i' = sub
applySubstitution sub (UKindFn a b)  = 
  UKindFn (applySubstitution sub a) (applySubstitution sub b)
applySubstitution _ other = other 

applySubstitutions :: (Ord i) => M.Map i (UKind i) -> UKind i -> UKind i
applySubstitutions subs kind = foldr' applySubstitution kind (M.toList subs)  

solve ::
  ( MonadError (KindError i) m, Ord i
  ) =>
  [(UKind i, UKind i)] ->
  m (M.Map i (UKind i)) 
solve = go mempty
  where
    go s [] = pure s
    go s1 (constraint : rest) =
      case constraint of
        (a, b) -> do
          s2 <- unifyKinds a b
          go (s2 <> s1) (applyToConstraint (s1 <> s2) <$> rest)

applyToConstraint :: (Ord i) => M.Map i (UKind i) -> (UKind i,UKind i) -> (UKind i, UKind i)
applyToConstraint subs (a, b) =
  (applySubstitutions subs a, applySubstitutions subs b)

unifyKinds :: (MonadError (KindError i) m, Ord i) => UKind i -> UKind i -> m (M.Map i (UKind i)) 
unifyKinds a b | a ==b = pure mempty
unifyKinds (UVar i) b = pure $ M.singleton i b 
unifyKinds a (UVar i) = pure $ M.singleton i a
unifyKinds (UKindFn argA retA) (UKindFn argB retB) = do
  (<>) <$> unifyKinds argA argB <*> unifyKinds retA retB
unifyKinds a b = throwError (KindMismatch a b)


getUnique :: (MonadState (KindState dep ann) m) => m Int
getUnique = do
  i <- gets ksInt
  modify (\ks -> ks {ksInt = i + 1})
  pure i

addConstraint :: (MonadState (KindState dep ann) m) => UKind Int -> UKind Int -> m ()
addConstraint expected actual =
  modify (\ks -> ks {ksEnv = (expected, actual) : ksEnv ks})

inferKinds ::
  ( MonadState (KindState dep ann) m,
    Ord (dep TypeName),
    Show (dep TypeName)
  ) =>
  Type dep ann ->
  m (Type dep (UKind Int))
inferKinds (TPrim _ p) = pure $ TPrim UStar p
inferKinds (TApp _ fn arg) = do
  fnKind <- inferKinds fn
  argKind <- inferKinds arg
  k <- case getTypeAnnotation fnKind of
    UKindFn _ r -> pure r
    _ -> do
      var <- UVar <$> getUnique
      pure var
  kArg <- UVar <$> getUnique

  -- we don't know what l is, but we know it's `* -> r`
  addConstraint (UKindFn UStar (getTypeAnnotation argKind)) kArg

  TApp k (fnKind $> kArg) <$> inferKinds arg
inferKinds (TConstructor _ constructor) = do
  dts <- gets ksDataTypes
  let k = case M.lookup constructor dts of
        Just dt -> foldl' (\kind _ -> UKindFn UStar kind) UStar (dtVars dt)
        Nothing -> error $ "Could not find data type for " <> show constructor
  pure $ TConstructor k constructor
inferKinds (TVar _ var) = do
  i <- getUnique
  pure $ TVar (UVar i) var
inferKinds (TLiteral _ l) = pure (TLiteral UStar l)
inferKinds (TFunc _ env a b) =
  TFunc UStar <$> traverse inferKinds env <*> inferKinds a <*> inferKinds b
inferKinds (TTuple _ a as) =
  TTuple UStar <$> inferKinds a <*> traverse inferKinds as
inferKinds (TArray _ a as) = TArray UStar a <$> inferKinds as
inferKinds (TUnknown _ a) = pure (TUnknown UStar a)
inferKinds (TRecord _ as) = TRecord UStar <$> traverse inferKinds as
inferKinds (TInfix _ op a b) = TInfix UStar op <$> inferKinds a <*> inferKinds b

lookupKindInType ::
  ( Eq (dep Identifier)
  ) =>
  Type dep Kind ->
  dep Identifier ->
  Maybe Kind
lookupKindInType ty identifier =
  listToMaybe $ go ty
  where
    go (TVar k a) | a == identifier = [k]
    go other = monoidType go other
