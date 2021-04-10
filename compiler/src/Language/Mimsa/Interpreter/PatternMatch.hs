{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.PatternMatch
  ( patternMatch,
  )
where

import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers

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
findConstructor (MyLiteral _ (MyString s)) =
  if stringLength s == 0
    then pure "StrEmpty"
    else pure "StrHead"
findConstructor (MyArray _ items) =
  if null items
    then pure "ArrEmpty"
    else pure "ArrHead"
findConstructor e = throwError $ PatternMatchFailure e

-- apply each part of the constructor to the output function
createMatchExpression ::
  (Monoid ann) =>
  Expr Variable ann ->
  Expr Variable ann ->
  Expr Variable ann
createMatchExpression f (MyConsApp _ c a) =
  MyApp mempty (createMatchExpression f c) a
createMatchExpression f (MyLiteral _ (MyString s)) =
  case stringSplit s of
    Just (sHead, sTail) ->
      MyApp
        mempty
        ( MyApp
            mempty
            f
            (MyLiteral mempty (MyString sHead))
        )
        (MyLiteral mempty (MyString sTail))
    Nothing -> f
createMatchExpression f (MyArray _ as) =
  if (not . null) as
    then
      MyApp
        mempty
        ( MyApp
            mempty
            f
            (head as)
        )
        (MyArray mempty (tail as))
    else f
createMatchExpression f _ = f
