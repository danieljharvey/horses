{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.SwapName
  ( swapName,
  )
where

import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types

-- step through Expr, replacing vars with numbered variables
swapName :: Variable -> Variable -> Expr Variable -> App (Expr Variable)
swapName from to (MyVar from') =
  pure $
    if from == from'
      then MyVar to
      else MyVar from'
swapName from to (MyLet name a b) =
  MyLet <$> pure name
    <*> swapName from to a
    <*> swapName from to b
swapName from to (MyLambda name a) =
  MyLambda <$> pure name <*> swapName from to a
swapName from to (MyRecordAccess a name) =
  MyRecordAccess <$> swapName from to a <*> pure name
swapName from to (MyApp a b) =
  MyApp <$> swapName from to a
    <*> swapName from to b
swapName from to (MyIf a b c) =
  MyIf
    <$> swapName from to a
      <*> swapName from to b
      <*> swapName from to c
swapName from to (MyPair a b) =
  MyPair
    <$> swapName from to a <*> swapName from to b
swapName from to (MyLetPair nameA nameB a b) =
  MyLetPair
    <$> pure nameA <*> pure nameB
      <*> swapName from to a
      <*> swapName from to b
swapName from to (MyLetList nameHead nameRest a b) =
  MyLetList <$> pure nameHead
    <*> pure nameRest
    <*> swapName from to a
    <*> swapName from to b
swapName from to (MySum side a) =
  MySum
    <$> pure side
      <*> swapName from to a
swapName from to (MyCase a b c) =
  MyCase <$> swapName from to a <*> swapName from to b
    <*> swapName from to c
swapName from to (MyList as) = do
  mas <- traverse (swapName from to) as
  pure (MyList mas)
swapName from to (MyRecord map') = do
  map2 <- traverse (swapName from to) map'
  pure (MyRecord map2)
swapName _ _ (MyLiteral a) = pure (MyLiteral a)
