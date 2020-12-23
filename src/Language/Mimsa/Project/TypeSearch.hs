{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Project.TypeSearch
  ( NormalisedMonoType (..),
    typeSearch,
    typeSearchFromText,
  )
where

import Data.Bifunctor (first)
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Actions (resolveStoreExpression)
import Language.Mimsa.Parser.Helpers (parseAndFormat)
import Language.Mimsa.Parser.MonoType
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
filterTypes items mt = M.filter (\mt' -> Normalised mt' == Normalised mt) items

-- | Typecheck everything in store and put in a map with binding name
-- | Ideally we should move this out and cache type sigs of each store
-- expression
resolvedDepsToTypeMap ::
  Store Annotation ->
  ResolvedDeps Annotation ->
  TsApp Annotation (Map Name MonoType)
resolvedDepsToTypeMap store' deps = do
  let getType se =
        TsApp $
          resolveStoreExpression store' mempty se
            >>= \(ResolvedExpression mt _ _ _ _) -> pure mt
  listItems <-
    traverse
      (\(name, (_, se)) -> (,) name <$> getType se)
      (M.toList $ getResolvedDeps deps)
  pure (M.fromList listItems)

typeSearch ::
  Store Annotation ->
  ResolvedDeps Annotation ->
  MonoType ->
  Either (Error Annotation) (Map Name MonoType)
typeSearch store' bindings' mt = do
  items <- runTsApp $ resolvedDepsToTypeMap store' bindings'
  pure (filterTypes items mt)

typeSearchFromText :: Store Annotation -> ResolvedDeps Annotation -> Text -> Either (Error Annotation) (Map Name MonoType)
typeSearchFromText store' deps input = do
  mt <- first OtherError (parseAndFormat monoTypeParser input)
  typeSearch store' deps mt
