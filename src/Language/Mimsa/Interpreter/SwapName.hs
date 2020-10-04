{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.SwapName
  ( swapName,
  )
where

import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types

-- step through Expr, replacing vars with numbered variables
swapName :: Variable -> Variable -> Expr ann Variable -> App ann (Expr ann Variable)
swapName from to (MyVar ann from') =
  pure $
    if from == from'
      then MyVar ann to
      else MyVar ann from'
swapName from to (MyLet ann name a b) =
  MyLet ann name
    <$> swapName from to a
    <*> swapName from to b
swapName from to (MyLambda ann name a) =
  MyLambda ann name <$> swapName from to a
swapName from to (MyRecordAccess ann a name) =
  MyRecordAccess ann <$> swapName from to a <*> pure name
swapName from to (MyApp ann a b) =
  MyApp ann <$> swapName from to a
    <*> swapName from to b
swapName from to (MyIf ann a b c) =
  MyIf ann
    <$> swapName from to a
      <*> swapName from to b
      <*> swapName from to c
swapName from to (MyPair ann a b) =
  MyPair ann
    <$> swapName from to a <*> swapName from to b
swapName from to (MyLetPair ann nameA nameB a b) =
  MyLetPair ann nameA nameB
    <$> swapName from to a
    <*> swapName from to b
swapName from to (MyRecord ann map') = do
  map2 <- traverse (swapName from to) map'
  pure (MyRecord ann map2)
swapName _ _ (MyLiteral ann a) = pure (MyLiteral ann a)
swapName from to (MyData ann dataType expr) =
  MyData ann dataType <$> swapName from to expr
swapName _ _ (MyConstructor ann n) = pure (MyConstructor ann n)
swapName from to (MyConsApp ann a b) =
  MyConsApp ann <$> swapName from to a
    <*> swapName from to b
swapName from to (MyCaseMatch ann expr matches catchAll) = do
  expr' <- swapName from to expr
  matches' <- traverse (\(k, v) -> (,) k <$> swapName from to v) matches
  catchAll' <- traverse (swapName from to) catchAll
  pure (MyCaseMatch ann expr' matches' catchAll')
