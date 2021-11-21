{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Actions.Helpers.Swaps
  ( useSwaps,
  )
where

import Control.Monad.Except
import Data.Functor
import Language.Mimsa.Actions.Types
import qualified Language.Mimsa.Interpreter.UseSwaps as Swaps
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker

useSwaps ::
  Swaps ->
  Expr Variable MonoType ->
  ActionM (Expr Name MonoType)
useSwaps swaps expr =
  case Swaps.useSwaps swaps expr of
    Right tyExpr -> pure tyExpr
    Left e -> throwError (InterpreterErr (e $> mempty))
