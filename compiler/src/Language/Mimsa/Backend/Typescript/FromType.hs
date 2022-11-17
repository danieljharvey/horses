{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Typescript.FromType (toTSType, toTSType') where

import qualified Data.List.NonEmpty as NE
import Control.Monad.Except
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Mimsa.Backend.Typescript.Monad
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

consToTSType :: Type ann -> TypescriptM (TSType, Set TSGeneric)
consToTSType mt =
  case varsFromDataType mt of
    Just (_modName, TypeName n, vars) -> do
      imported <- typeNameIsImport (TypeName n)
      let namespace =
            if imported
              then Just n
              else Nothing
      tsTypes <- traverse toTSType vars
      let (types, generics) = unzip tsTypes
      pure (TSType namespace n types, mconcat generics)
    Nothing ->
      throwError NoConstructorInTypeApp

toTSTypeRecord :: Map Name (Type ann) -> TypescriptM (TSType, Set TSGeneric)
toTSTypeRecord as = do
  tsAll <- traverse toTSType as
  let generics = snd . snd <$> M.toList tsAll
      tsItems = M.fromList . fmap (bimap coerce fst) . M.toList $ tsAll
  pure (TSTypeRecord tsItems, mconcat generics)

toTSTypeTuple :: [Type ann] -> TypescriptM (TSType, Set TSGeneric)
toTSTypeTuple as = do
  tsAll <- traverse toTSType as
  let generics = snd  <$> tsAll
      tsItems = fst <$> tsAll
  pure (TSTypeTuple tsItems, mconcat generics)

toTSType :: Type ann -> TypescriptM (TSType, Set TSGeneric)
toTSType = toTSType' False

-- | returns the type and any generics used in the expression
toTSType' :: Bool -> Type ann -> TypescriptM (TSType, Set TSGeneric)
toTSType' _ (MTPrim _ MTString) = pure (TSType Nothing "string" [], mempty)
toTSType' _ (MTPrim _ MTInt) = pure (TSType Nothing "number" [], mempty)
toTSType' _ (MTPrim _ MTBool) = pure (TSType Nothing "boolean" [], mempty)
toTSType' _ (MTVar _ a) =
  let newVar = case a of
        TVUnificationVar i' -> T.toTitle (T.pack (printTypeNum (i' + 1)))
        TVName a' -> T.toTitle (coerce a')
        TVScopedVar i' _ -> T.toTitle (T.pack (printTypeNum (i' + 1)))
   in pure (TSTypeVar newVar, S.singleton (TSGeneric newVar))
toTSType' _ mt@MTTypeApp {} =
  consToTSType mt
toTSType' topLevel (MTFunction _ a b) = do
  (tsA, genA) <- toTSType' False a
  (tsB, genB) <- toTSType' False b
  let generics =
        if topLevel
          then genA -- we don't want to include generics from later args in curried functions
          else genA <> genB -- but we do want later args of higher-order functions
  pure (TSTypeFun "arg" tsA tsB, generics)
toTSType' _ (MTArray _ as) = do
  (tsAs, genAs) <- toTSType as
  pure (TSTypeArray tsAs, genAs)
toTSType' _ (MTTuple _ a as) = do
  toTSTypeTuple ([a] <> NE.toList as)
toTSType' _ mt@MTConstructor {} =
  consToTSType mt
toTSType' _ (MTRecord _ as Nothing) =
  toTSTypeRecord as
toTSType' _ (MTRecord _ as (Just rest)) = do
  (tsItems, generics) <- toTSTypeRecord as
  (tsRest, genRest) <- toTSType rest
  pure (TSTypeAnd tsItems tsRest, generics <> genRest)
toTSType' b (MTGlobals _ _ _ expr) = toTSType' b expr
