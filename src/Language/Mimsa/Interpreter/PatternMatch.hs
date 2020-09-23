{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.PatternMatch (patternMatch) where

import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types

patternMatch ::
  Expr Variable ->
  NonEmpty (TyCon, Expr Variable) ->
  Maybe (Expr Variable) ->
  App (Expr Variable)
patternMatch expr' options catchAll = do
  const' <- findConstructor expr'
  case NE.filter (\(c, _) -> c == const') options of
    [(_, found)] -> pure $ createMatchExpression found expr'
    _ ->
      case catchAll of
        Just catchAll' -> pure catchAll'
        _ -> throwError $ PatternMatchFailure expr'

-- when given an applied constructor, find the name for matching
findConstructor :: Expr Variable -> App TyCon
findConstructor (MyConstructor c) = pure c
findConstructor (MyConsApp c _) = findConstructor c
findConstructor e = throwError $ PatternMatchFailure e

-- apply each part of the constructor to the output function
createMatchExpression :: Expr Variable -> Expr Variable -> Expr Variable
createMatchExpression f (MyConsApp c a) = MyApp (createMatchExpression f c) a
createMatchExpression f _ = f
