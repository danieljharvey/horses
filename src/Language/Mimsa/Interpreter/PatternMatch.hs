{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.PatternMatch where

import Control.Monad.Except
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Logging
import Language.Mimsa.Types

patternMatch ::
  Expr Variable ->
  [(Construct, Expr Variable)] ->
  Maybe (Expr Variable) ->
  App (Expr Variable)
patternMatch expr' options catchAll = do
  const' <- findConstructor expr'
  case filter (\(c, _) -> c == const') options of
    [(_, found)] -> pure $ createMatchExpression found $ debugPretty "expr" expr'
    _ ->
      case catchAll of
        Just catchAll' -> pure catchAll'
        _ -> throwError $ PatternMatchFailure expr'

-- when given an applied constructor, find the name for matching
findConstructor :: Expr Variable -> App Construct
findConstructor (MyConstructor c) = pure c
findConstructor (MyConsApp c _) = findConstructor c
findConstructor e = throwError $ PatternMatchFailure e

-- apply each part of the constructor to the output function
createMatchExpression :: Expr Variable -> Expr Variable -> Expr Variable
createMatchExpression f (MyConsApp c a) = MyApp (createMatchExpression f c) a
createMatchExpression f _ = f
