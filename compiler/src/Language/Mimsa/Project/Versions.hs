{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.Versions
  ( findVersions,
    findVersionsSimple,
    findStoreExpressionByName,
  )
where

import Control.Monad.Except
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Set (Set)
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Project.Usages
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker

-- find which versions of a given binding are in use

-- | if we don't need to show stuff this is straightforward
findVersionsSimple ::
  Project ann ->
  Name ->
  Either
    (Error ann)
    ( NonEmpty
        ( Int,
          ExprHash,
          Set Usage
        )
    )
findVersionsSimple project name = do
  versioned <- first StoreErr (findInProject project name)
  let findUse hash =
        (,) hash <$> first StoreErr (findUsages project hash)
  as <- traverse findUse versioned
  let numbered = NE.zip (NE.fromList [1 ..]) as
  pure $
    NE.reverse $
      (\(i, (hash, usages)) -> (i, hash, usages)) <$> numbered

findVersions ::
  Project Annotation ->
  Name ->
  Either
    (Error Annotation)
    ( NonEmpty
        ( Int,
          Expr Variable Annotation,
          MonoType,
          Set Usage,
          ExprHash
        )
    )
findVersions project name = do
  versioned <- first StoreErr (findInProject project name)
  as <- traverse (getExprDetails project) versioned
  let numbered = NE.zip (NE.fromList [1 ..]) as
  pure $ NE.reverse $ (\(i, (a, b, c, d)) -> (i, a, b, c, d)) <$> numbered

findInProject :: Project ann -> Name -> Either StoreError (NonEmpty ExprHash)
findInProject project name =
  case M.lookup name (getVersionedMap $ prjBindings project) of
    Just versioned -> Right versioned
    _ -> throwError $ CouldNotFindBinding name

getStoreExpression ::
  Project ann ->
  ExprHash ->
  Either StoreError (StoreExpression ann)
getStoreExpression (Project store' _ _ _) exprHash =
  case M.lookup exprHash (getStore store') of
    Just storeExpression' -> Right storeExpression'
    _ -> Left (CouldNotFindStoreExpression exprHash)

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing

findStoreExpressionByName :: Project ann -> Name -> Maybe (StoreExpression ann)
findStoreExpressionByName env name =
  case findInProject env name of
    Right hashes -> hush $ getStoreExpression env (NE.last hashes)
    _ -> Nothing

getExprDetails ::
  Project Annotation ->
  ExprHash ->
  Either (Error Annotation) (Expr Variable Annotation, MonoType, Set Usage, ExprHash)
getExprDetails project exprHash = do
  usages <-
    first StoreErr (findUsages project exprHash)
  storeExpr <-
    first StoreErr (getStoreExpression project exprHash)
  typeMap <- Actions.getTypeMap project
  resolvedExpr <-
    Actions.resolveStoreExpression (prjStore project) typeMap "" storeExpr
  pure (reExpression resolvedExpr, reMonoType resolvedExpr, usages, exprHash)
