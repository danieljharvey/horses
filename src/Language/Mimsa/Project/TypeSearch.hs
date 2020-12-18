{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Project.TypeSearch
  ( NormalisedMonoType (..),
    typeSearch,
  )
where

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Actions (resolveStoreExpression)
import Language.Mimsa.Typechecker.NormaliseTypes
import Language.Mimsa.Types.AST.Annotation
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

newtype NormalisedMonoType
  = Normalised MonoType

instance Eq NormalisedMonoType where
  (Normalised mtA) == (Normalised mtB) =
    normalise mtA == normalise mtB
    where
      normalise a =
        normaliseType a $> ()

newtype TsApp ann a = TsApp {runTsApp :: Either (Error ann) a}
  deriving newtype (Functor, Applicative, Monad)

filterTypes :: Map Name MonoType -> MonoType -> Map Name MonoType
filterTypes items mt = M.filter (== mt) items

-- | Typecheck everything in store and put in a map with binding name
-- | Ideally we should move this out and cache type sigs of each store
-- expression
resolvedDepsToTypeMap :: ResolvedDeps Annotation -> TsApp Annotation (Map Name MonoType)
resolvedDepsToTypeMap deps = do
  let store' = storeFromResolvedDeps deps
  let getType se =
        TsApp $
          resolveStoreExpression store' mempty se
            >>= \(ResolvedExpression mt _ _ _ _) -> pure mt
  listItems <-
    traverse
      (\(name, (_, se)) -> (,) name <$> getType se)
      (M.toList $ getResolvedDeps deps)
  pure (M.fromList listItems)

-- | this is stupid, we should be using ResolvedDeps throughout the app
storeFromResolvedDeps :: ResolvedDeps ann -> Store ann
storeFromResolvedDeps (ResolvedDeps deps) = Store $ M.fromList (M.elems deps)

typeSearch :: ResolvedDeps Annotation -> MonoType -> Map Name MonoType
typeSearch deps mt = case runTsApp $
  resolvedDepsToTypeMap deps of
  Right items -> filterTypes items mt
  Left _ -> mempty
