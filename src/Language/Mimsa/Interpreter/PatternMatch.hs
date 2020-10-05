{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.PatternMatch
  ( patternMatch,
  )
where

import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types

patternMatch ::
  (Monoid ann) =>
  Expr Variable ann ->
  NonEmpty (TyCon, Expr Variable ann) ->
  Maybe (Expr Variable ann) ->
  App ann (Expr Variable ann)
patternMatch expr' options catchAll = do
  const' <- findConstructor expr'
  case NE.filter (\(c, _) -> c == const') options of
    [(_, found)] -> pure $ createMatchExpression found expr'
    _ ->
      case catchAll of
        Just catchAll' -> pure catchAll'
        _ -> throwError $ PatternMatchFailure expr'

-- when given an applied constructor, find the name for matching
findConstructor :: Expr Variable ann -> App ann TyCon
findConstructor (MyConstructor _ c) = pure c
findConstructor (MyConsApp _ c _) = findConstructor c
findConstructor e = throwError $ PatternMatchFailure e

-- apply each part of the constructor to the output function
createMatchExpression ::
  (Monoid ann) =>
  Expr Variable ann ->
  Expr Variable ann ->
  Expr Variable ann
createMatchExpression f (MyConsApp _ c a) = MyApp mempty (createMatchExpression f c) a
createMatchExpression f _ = f
