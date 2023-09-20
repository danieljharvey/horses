{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
  {-# LANGUAGE LambdaCase #-}
module Smol.Core.Typecheck.Typeclass.KindChecker (Kind (..), typeKind, lookupKindInType) where

import Data.Functor
import Debug.Trace
import Smol.Core.Printer
import Control.Monad.State
import Data.Foldable
import Smol.Core.TypeUtils (monoidType)
import Data.Monoid
import Smol.Core.Typecheck.Annotations
import qualified Data.Map as M
import Smol.Core.Types.DataType
import Smol.Core.Types.Type
import Smol.Core.Types.TypeName
import Smol.Core.Types.Identifier
import qualified Prettyprinter as PP

data Kind
  = Star
  | KindFn Kind Kind
  deriving stock (Eq, Ord, Show)

-- | Unresolved Kind
data UKind i =
    UStar | UKindFn (UKind i) (UKind i) | UVar i
  deriving stock (Eq,Ord,Show)

instance (Show i) => Printer (UKind i) where
  prettyDoc = PP.pretty . show

data KindState dep ann = KindState
  { ksDataTypes :: M.Map (dep TypeName) (DataType dep ann),
    ksInt :: Int,
    ksEnv :: [(Int, UKind Int)] -- unique, what it is
  }


runConstraint :: (UKind i, UKind i) => Type (UKind i) ann -> Type (UKind i) ann
runConstraint (expected, actual) ty =
  go ty
    where
      go (TVar k



typeKind :: (Ord (dep TypeName), Show (dep Identifier), Show (dep TypeName)) => M.Map (dep TypeName) (DataType dep ann) ->
  Type dep ann -> Type dep Kind
typeKind dts = fmap toKind . traceShowId . flip evalState (KindState dts 1 mempty) . inferKinds

toKind :: (Show i) => UKind i -> Kind
toKind UStar = Star
toKind (UKindFn a b) =KindFn (toKind a) (toKind b)
toKind (UVar i) = error $ "Found UVar " <> show i

getUnique :: (MonadState (KindState dep ann) m) => m Int
getUnique = do
  i <- gets ksInt
  modify (\ks -> ks { ksInt = i + 1 })
  pure i

addConstraint :: (MonadState (KindState dep ann) m) => UKind Int -> UKind Int -> m ()
addConstraint expected actual
  = modify (\ks -> ks { ksEnv = (expected, actual) : ksEnv ks } )

inferKinds :: (MonadState (KindState dep ann) m,
    Ord (dep TypeName), Show (dep TypeName)) => Type dep ann -> m (Type dep (UKind Int))
inferKinds (TPrim _ p) = pure $ TPrim UStar p
inferKinds (TApp _ fn arg) = do
  fnKind <- inferKinds fn
  argKind <- inferKinds arg
  k <- case getTypeAnnotation fnKind of
        UKindFn _ r -> pure r
        _ -> do
          var <- UVar <$> getUnique
          pure var
  unique <- getUnique
  let l = UVar unique

  -- we don't know what l is, but we know it's `* -> r`
  addConstraint (UKindFn UStar (getTypeAnnotation argKind)) l

  TApp k (fnKind $> l) <$> inferKinds arg
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

lookupKindInType :: (
    Eq (dep Identifier)) =>
    Type dep Kind -> dep Identifier -> Maybe Kind
lookupKindInType ty identifier 
  = getFirst $ monoidType (\case 
          TVar k a | a == identifier -> First (Just k)
          _ -> First Nothing) ty

