{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Project.Versions
  ( findVersions,
    findVersionsSimple,
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

-- versions of a binding along with numbers
findVersionsSimple ::
  Project ann ->
  Name ->
  Either
    (Error ann)
    ( NonEmpty
        ( Int,
          ExprHash
        )
    )
findVersionsSimple project name = do
  versioned <- first StoreErr (findInProject project name)
  pure . NE.reverse . NE.zip (NE.fromList [1 ..]) $ versioned

-- find which versions of a given binding are in use and resolve them to
-- expressions
findVersions ::
  Project Annotation ->
  Name ->
  Either
    (Error Annotation)
    ( NonEmpty
        ( Int,
          Expr Name Annotation,
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

findInProject ::
  Project ann ->
  Name ->
  Either StoreError (NonEmpty ExprHash)
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

getExprDetails ::
  Project Annotation ->
  ExprHash ->
  Either
    (Error Annotation)
    (Expr Name Annotation, MonoType, Set Usage, ExprHash)
getExprDetails project exprHash = do
  usages <-
    first StoreErr (findUsages project exprHash)
  storeExpr <-
    first StoreErr (getStoreExpression project exprHash)
  typeMap <- Actions.getTypeMap project
  resolvedExpr <-
    Actions.resolveStoreExpression (prjStore project) typeMap "" storeExpr
  pure
    ( storeExpression (reStoreExpression resolvedExpr),
      reMonoType resolvedExpr,
      usages,
      exprHash
    )
