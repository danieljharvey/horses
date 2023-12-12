{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Typecheck.Typeclass.KindChecker
  ( module Smol.Typecheck.Typeclass.Types.Kind,
    fromKind,
    toKind,
    unifyKinds,
    typeKind,
    lookupKindInType,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Smol.Core.Annotations
import Smol.Core.TypeUtils (monoidType)
import Smol.Core.Types.DataType
import Smol.Core.Types.Identifier
import Smol.Core.Types.Type
import Smol.Core.Types.TypeName
import Smol.Typecheck.Typeclass.Types.Kind

data KindState dep ann = KindState
  { ksDataTypes :: M.Map (dep TypeName) (DataType dep ann),
    ksInt :: Int,
    ksEnv :: [(UKind Int, UKind Int)] -- unique, what it is
  }

fromKind :: Kind -> UKind i
fromKind Star = UStar
fromKind (KindFn a b) = UKindFn (fromKind a) (fromKind b)

toKind :: (Show i) => UKind i -> Kind
toKind UStar = Star
toKind (UKindFn a b) = KindFn (toKind a) (toKind b)
toKind (UVar _i) = Star -- yolo, if we've not found any better news, assume its a Type

typeKind ::
  ( MonadError (KindError dep Int) m,
    Ord (dep TypeName),
    Show (dep TypeName),
    Show (dep Identifier)
  ) =>
  M.Map (dep TypeName) (DataType dep ann) ->
  Type dep ann ->
  m (Type dep Kind)
typeKind dts ty = do
  (ty', ks) <- runStateT (inferKinds ty) (KindState dts 1 mempty)
  subs <- solve (ksEnv ks)
  unifyType subs ty'

-- given a bunch of substitutions
-- run them all
unifyType ::
  (MonadError (KindError dep Int) m) =>
  M.Map Int (UKind Int) ->
  Type dep (UKind Int) ->
  m (Type dep Kind)
unifyType subs ty = do
  let tyWithKinds = applySubstitutions subs <$> ty

  pure $ fmap toKind tyWithKinds

applySubstitution :: (Eq i) => (i, UKind i) -> UKind i -> UKind i
applySubstitution (i, sub) (UVar i') | i == i' = sub
applySubstitution sub (UKindFn a b) =
  UKindFn (applySubstitution sub a) (applySubstitution sub b)
applySubstitution _ other = other

applySubstitutions :: (Ord i) => M.Map i (UKind i) -> UKind i -> UKind i
applySubstitutions subs kind =
  foldl' (flip applySubstitution) kind (M.toList subs)

solve ::
  ( MonadError (KindError dep i) m,
    Ord i
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
          go (s2 <> s1) (applyToConstraint (s2 <> s1) <$> rest)

applyToConstraint :: (Ord i) => M.Map i (UKind i) -> (UKind i, UKind i) -> (UKind i, UKind i)
applyToConstraint subs (a, b) =
  (applySubstitutions subs a, applySubstitutions subs b)

unifyKinds :: (MonadError (KindError dep i) m, Ord i) => UKind i -> UKind i -> m (M.Map i (UKind i))
unifyKinds a b | a == b = pure mempty
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
    MonadError (KindError dep Int) m,
    Ord (dep TypeName),
    Show (dep TypeName),
    Show (dep Identifier)
  ) =>
  Type dep ann ->
  m (Type dep (UKind Int))
inferKinds (TPrim _ p) = pure $ TPrim UStar p
inferKinds (TApp _ fn arg) = do
  argKind <- checkKinds arg

  fnKind <- inferKinds fn

  resultKind <- UVar <$> getUnique

  let lhs = UKindFn (getTypeAnnotation argKind) resultKind
      rhs = getTypeAnnotation fnKind

  -- tell whatever we guessed fn was that in fact it's some kind of `UKindFn`
  addConstraint lhs rhs

  pure $ TApp resultKind fnKind argKind
inferKinds (TConstructor _ constructor) = do
  dts <- gets ksDataTypes
  k <- case M.lookup constructor dts of
    Just dt -> pure $ foldl' (\kind _ -> UKindFn UStar kind) UStar (dtVars dt)
    Nothing -> throwError (MissingDataType constructor)
  pure $ TConstructor k constructor
inferKinds (TVar _ var) = do
  i <- getUnique
  pure $ TVar (UVar i) var
inferKinds (TLiteral _ l) = pure (TLiteral UStar l)
inferKinds (TFunc _ env a b) =
  TFunc UStar <$> traverse checkKinds env <*> checkKinds a <*> checkKinds b
inferKinds (TTuple _ a as) =
  TTuple UStar <$> checkKinds a <*> traverse checkKinds as
inferKinds (TArray _ a as) = TArray UStar a <$> checkKinds as
inferKinds (TUnknown _ a) = pure (TUnknown UStar a)
inferKinds (TRecord _ as) = TRecord UStar <$> traverse checkKinds as
inferKinds (TInfix _ op a b) = TInfix UStar op <$> checkKinds a <*> checkKinds b

-- infer and emit constraint that this is always Star
checkKinds ::
  ( Ord (dep TypeName),
    Show (dep TypeName),
    Show (dep Identifier),
    MonadState (KindState dep ann) m,
    MonadError (KindError dep Int) m
  ) =>
  Type dep ann ->
  m (Type dep (UKind Int))
checkKinds ty = do
  tyKind <- inferKinds ty
  addConstraint UStar (getTypeAnnotation tyKind)
  pure tyKind

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
