{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Actions.Helpers.NumberStoreExpression (numberStoreExpression) where

import Control.Monad.Except
import Data.Bifunctor
import Data.Map.Strict (Map)
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.NumberVars
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker.Unique

numberStoreExpression ::
  (MonadError (Error ann) m) =>
  Expr Name Annotation ->
  Map (Maybe ModuleName, Name) ExprHash ->
  m (Expr (Name, Unique) Annotation)
numberStoreExpression expr bindings =
  -- add numbers and mark imports
  liftEither
    ( first
        (TypeErr (prettyPrint expr))
        (addNumbersToStoreExpression expr bindings)
    )
