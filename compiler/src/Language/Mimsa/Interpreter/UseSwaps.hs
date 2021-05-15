{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.UseSwaps (useSwaps) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Swaps

type App ann = ReaderT Swaps (Either (InterpreterError ann))

lookupSwap :: Variable -> App ann Name
lookupSwap var = do
  swaps <- ask
  case M.lookup var swaps of
    Just a -> pure a
    _ -> throwError $ CouldNotFindSwapForVariable var swaps

useSwaps ::
  Swaps ->
  Expr Variable ann ->
  Either (InterpreterError ann) (Expr Name ann)
useSwaps swaps expr' = runReaderT (useSwaps' expr') swaps

useSwaps' :: Expr Variable ann -> App ann (Expr Name ann)
useSwaps' (MyLambda ann var body) =
  MyLambda ann <$> lookupSwap var <*> useSwaps' body
useSwaps' (MyVar ann var) =
  MyVar ann <$> lookupSwap var
useSwaps' (MyLet ann var expr' body) = do
  MyLet ann <$> lookupSwap var
    <*> useSwaps' expr'
    <*> useSwaps' body
useSwaps' (MyLetPair ann varA varB a b) =
  MyLetPair
    ann
    <$> lookupSwap varA
    <*> lookupSwap varB
    <*> useSwaps' a
    <*> useSwaps' b
useSwaps' (MyInfix ann op a b) =
  MyInfix ann op <$> useSwaps' a <*> useSwaps' b
useSwaps' (MyRecordAccess ann a name) =
  MyRecordAccess ann
    <$> useSwaps' a <*> pure name
useSwaps' (MyApp ann a b) =
  MyApp ann <$> useSwaps' a <*> useSwaps' b
useSwaps' (MyIf ann a b c) =
  MyIf ann <$> useSwaps' a <*> useSwaps' b <*> useSwaps' c
useSwaps' (MyPair ann a b) =
  MyPair ann <$> useSwaps' a <*> useSwaps' b
useSwaps' (MyRecord ann map') = do
  map2 <- traverse useSwaps' map'
  pure (MyRecord ann map2)
useSwaps' (MyArray ann map') = do
  map2 <- traverse useSwaps' map'
  pure (MyArray ann map2)
useSwaps' (MyLiteral ann a) = pure (MyLiteral ann a)
useSwaps' (MyData ann dt b) =
  MyData ann dt <$> useSwaps' b
useSwaps' (MyConstructor ann name) = pure (MyConstructor ann name)
useSwaps' (MyConsApp ann fn var) = MyConsApp ann <$> useSwaps' fn <*> useSwaps' var
useSwaps' (MyCaseMatch ann expr' matches catchAll) = do
  let useSwapsPair (name, expr'') = (,) name <$> useSwaps' expr''
  matches' <- traverse useSwapsPair matches
  catchAll' <- traverse useSwaps' catchAll
  MyCaseMatch ann <$> useSwaps' expr' <*> pure matches' <*> pure catchAll'
useSwaps' (MyPatternMatch ann expr' patterns) = do
  let useSwapsPair (pat, expr'') = (,) <$> useSwapsInPattern pat <*> useSwaps' expr''
  patterns' <- traverse useSwapsPair patterns
  MyPatternMatch ann <$> useSwaps' expr' <*> pure patterns'
useSwaps' (MyTypedHole ann a) = pure $ MyTypedHole ann a
useSwaps' (MyDefineInfix ann infixOp bindName expr) =
  MyDefineInfix
    ann
    infixOp
    <$> lookupSwap bindName
    <*> useSwaps' expr

useSwapsInPattern :: Pattern Variable ann -> App ann (Pattern Name ann)
useSwapsInPattern _ = error "oh shit"
