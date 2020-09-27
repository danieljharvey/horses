{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Backend
  ( assembleJS,
  )
where

import Data.Bifunctor (first)
import Data.Coerce
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Store.ResolvedDeps (recursiveResolve)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.StoreExpression
import Language.Mimsa.Types.Usage

assemble ::
  (Monoid a) =>
  (Name -> Expr Name -> a) ->
  Store ->
  StoreExpression ->
  Name ->
  Either UsageError a
assemble render store storeExpr name = do
  deps <- first CouldNotResolveDeps $ recursiveResolve store storeExpr
  let renderWithName = uncurry render
  pure $ foldMap renderWithName deps <> render name (storeExpression storeExpr)

assembleJS :: Store -> StoreExpression -> Name -> Either UsageError Javascript
assembleJS store' storeExpr name' = do
  jsOutput <-
    assemble
      (\name expr -> "export const " <> coerce name <> " = " <> output expr <> ";\n")
      store'
      storeExpr
      name'
  pure $ jsStandardLibrary <> jsOutput
