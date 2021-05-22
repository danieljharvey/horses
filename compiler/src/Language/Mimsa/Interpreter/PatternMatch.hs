{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.PatternMatch
  ( patternMatch,
  )
where

import Control.Monad.Except
import Data.Foldable
import Data.Monoid
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers

patternMatch ::
  (Monoid ann) =>
  Expr Variable ann ->
  [(Pattern Variable ann, Expr Variable ann)] ->
  App ann (Expr Variable ann)
patternMatch expr' patterns = do
  let foldF (pat, patExpr) = case patternMatches pat expr' of
        Just bindings -> First (Just (patExpr, bindings))
        _ -> First Nothing
  case getFirst (foldMap foldF patterns) of
    Just (patExpr, bindings) -> pure $ createMatchExpression bindings patExpr
    _ ->
      throwError $ PatternMatchFailure expr'

-- pull vars out of expr to match patterns
patternMatches ::
  Pattern Variable ann ->
  Expr Variable ann ->
  Maybe [(Variable, Expr Variable ann)]
patternMatches (PWildcard _) _ = pure []
patternMatches (PVar _ name) expr = pure [(name, expr)]
patternMatches (PPair _ pA pB) (MyPair _ a b) = do
  as <- patternMatches pA a
  bs <- patternMatches pB b
  pure $ as <> bs
patternMatches _ _ = Nothing

-- apply each part of the constructor to the output function
createMatchExpression ::
  (Monoid ann) =>
  [(Variable, Expr Variable ann)] ->
  Expr Variable ann ->
  Expr Variable ann
createMatchExpression bindings a =
  foldl' (\rest (binder, var) -> MyLet mempty binder var rest) a bindings
