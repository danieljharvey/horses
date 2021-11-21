{-# LANGUAGE FlexibleContexts #-}
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

lookupSwap ::
  ( MonadReader Swaps m,
    MonadError (InterpreterError ann) m
  ) =>
  Variable ->
  m Name
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
useSwaps' (MyLambda ann (Identifier bindAnn var) body) =
  MyLambda ann <$> (Identifier bindAnn <$> lookupSwap var) <*> useSwaps' body
useSwaps' (MyVar ann var) =
  MyVar ann <$> lookupSwap var
useSwaps' (MyLet ann var expr' body) = do
  MyLet ann <$> lookupSwap var
    <*> useSwaps' expr'
    <*> useSwaps' body
useSwaps' (MyLetPattern ann pat expr body) = do
  newPat <- useSwapsInPattern pat
  MyLetPattern ann newPat <$> useSwaps' expr <*> useSwaps' body
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
useSwaps' (MyPatternMatch ann expr' patterns) = do
  let useSwapsPair (pat, expr'') =
        (,)
          <$> useSwapsInPattern pat <*> useSwaps' expr''
  patterns' <- traverse useSwapsPair patterns
  MyPatternMatch ann <$> useSwaps' expr' <*> pure patterns'
useSwaps' (MyTypedHole ann a) =
  pure $ MyTypedHole ann a
useSwaps' (MyDefineInfix ann infixOp bindExpr expr) =
  MyDefineInfix
    ann
    infixOp
    <$> useSwaps' bindExpr
    <*> useSwaps' expr

useSwapsInPattern :: Pattern Variable ann -> App ann (Pattern Name ann)
useSwapsInPattern (PWildcard ann) = pure (PWildcard ann)
useSwapsInPattern (PVar ann var) = PVar ann <$> lookupSwap var
useSwapsInPattern (PLit ann lit) = pure (PLit ann lit)
useSwapsInPattern (PConstructor ann tyCon args) =
  PConstructor ann tyCon
    <$> traverse useSwapsInPattern args
useSwapsInPattern (PPair ann a b) =
  PPair ann <$> useSwapsInPattern a
    <*> useSwapsInPattern b
useSwapsInPattern (PRecord ann as) =
  PRecord ann
    <$> traverse useSwapsInPattern as
useSwapsInPattern (PArray ann as spread) =
  PArray ann <$> traverse useSwapsInPattern as
    <*> useSwapsInSpread spread
useSwapsInPattern (PString ann a as) =
  PString ann <$> useSwapsInStringPart a
    <*> useSwapsInStringPart as

useSwapsInSpread :: Spread Variable ann -> App ann (Spread Name ann)
useSwapsInSpread NoSpread =
  pure NoSpread
useSwapsInSpread (SpreadWildcard ann) =
  pure (SpreadWildcard ann)
useSwapsInSpread (SpreadValue ann var) =
  SpreadValue ann <$> lookupSwap var

useSwapsInStringPart :: StringPart Variable ann -> App ann (StringPart Name ann)
useSwapsInStringPart (StrWildcard ann) = pure (StrWildcard ann)
useSwapsInStringPart (StrValue ann var) = StrValue ann <$> lookupSwap var
