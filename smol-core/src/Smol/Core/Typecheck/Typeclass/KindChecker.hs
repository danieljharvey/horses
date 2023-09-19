{-# LANGUAGE DerivingStrategies #-}
  {-# LANGUAGE LambdaCase #-}
module Smol.Core.Typecheck.Typeclass.KindChecker (Kind (..), typeKind, lookupKindInType) where

import Data.Foldable
import Smol.Core.TypeUtils (monoidType)
import Data.Monoid
import Data.Functor
import Smol.Core.Typecheck.Annotations
import qualified Data.Map as M
import Smol.Core.Types.DataType
import Smol.Core.Types.Type
import Smol.Core.Types.TypeName
import Smol.Core.Types.Identifier
import Debug.Trace

data Kind
  = Star
  | KindFn Kind Kind
  deriving stock (Eq, Ord, Show)

lookupKindInType :: (
    Eq (dep Identifier)) =>
    Type dep Kind -> dep Identifier -> Maybe Kind
lookupKindInType ty identifier 
  = getFirst $ monoidType (\case 
          TVar k a | a == identifier -> First (Just k)
          _ -> First Nothing) ty

-- | ahh fuck we need Kind vars and basic unification i think
typeKind ::
  ( Show ann, Show (dep TypeName),
    Show (dep Identifier),
    Ord (dep TypeName)
  ) =>
  M.Map (dep TypeName) (DataType dep ann) ->
  Type dep ann ->
  Type dep Kind
typeKind dts = 
  traceShowId . typeKind' . traceShowId
    where
      typeKind'  (TPrim _ p) = TPrim Star p
      typeKind' (TApp _ fn arg) =
        let k = case getTypeAnnotation (typeKind' fn) of
                    KindFn _ b -> b
                    other -> KindFn Star other 
        in TApp k (typeKind' fn $> k) (typeKind' arg)
      typeKind' (TConstructor _ constructor) =
        let k = case M.lookup constructor dts of
                  Just dt -> foldl' (\kind _ -> KindFn Star kind) Star (dtVars dt)
                  Nothing -> error $ "Could not find data type for " <> show constructor
        in TConstructor k constructor
      typeKind'  ty = ty $> Star

