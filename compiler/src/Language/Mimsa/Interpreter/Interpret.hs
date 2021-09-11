{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Interpreter.Interpret
  ( interpretWithScope,
  )
where

-- let's run our code, at least for the repl
-- run == simplify, essentially

import Control.Monad.Except
import Data.Functor
import qualified Data.Map as M
import Data.Maybe
import Language.Mimsa.ExprUtils
import Language.Mimsa.Interpreter.InstantiateVar
import Language.Mimsa.Interpreter.PatternMatch
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Scope

-- when we come to do let recursive the name of our binder
-- may already be turned into a number in the expr
-- so we look it up to make sure we bind the right thing
findActualBindingInSwaps :: Int -> App ann Variable
findActualBindingInSwaps int = do
  swaps <- askForSwaps
  scope' <- readScope
  case M.lookup (NumberedVar int) swaps of
    Just i' -> pure (NamedVar i')
    _ -> throwError $ CouldNotFindVar scope' (NumberedVar int)

useVar :: (Eq ann, Monoid ann) => Variable -> App ann (Expr Variable ann)
useVar var' = case var' of
  (NumberedVar i) -> do
    scope' <- readScope
    case M.lookup (NumberedVar i) (getScope scope') of
      Just expr ->
        instantiateVar expr
          >>= interpretWithScope
      Nothing -> do
        var <- findActualBindingInSwaps i -- try it by it's pre-substituted name before failing
        useVar var
  (NamedVar n) -> do
    scope' <- readScope
    case M.lookup (NamedVar n) (getScope scope') of
      Just expr -> do
        instantiateVar expr
          >>= interpretWithScope
      Nothing -> throwError $ CouldNotFindVar scope' (NamedVar n)

interpretStringConcat ::
  (Monoid ann) =>
  Expr Variable ann ->
  Expr Variable ann ->
  App ann (Expr Variable ann)
interpretStringConcat plainA plainB = do
  let withStr = pure . MyLiteral mempty . MyString . StringType
      getStr exp' = case exp' of
        (MyLiteral _ (MyString (StringType i))) -> Right i
        _ -> Left $ StringConcatenationFailure plainA plainB
  case (,) <$> getStr plainA <*> getStr plainB of
    Right (a', b') -> withStr (a' <> b')
    Left e -> throwError e

interpretArrayConcat ::
  (Monoid ann) =>
  Expr Variable ann ->
  Expr Variable ann ->
  App ann (Expr Variable ann)
interpretArrayConcat plainA plainB = do
  let withArr = pure . MyArray mempty
      getArr exp' = case exp' of
        (MyArray _ i) -> Right i
        _ -> Left $ ArrayConcatenationFailure plainA plainB
  case (,) <$> getArr plainA <*> getArr plainB of
    Right (a', b') -> withArr (a' <> b')
    Left e -> throwError e

interpretOperator ::
  (Eq ann, Monoid ann) =>
  Operator ->
  Expr Variable ann ->
  Expr Variable ann ->
  App ann (Expr Variable ann)
interpretOperator operator a b = do
  plainA <- interpretWithScope a
  plainB <- interpretWithScope b
  let removeAnn expr = expr $> ()
  case operator of
    Equals -> do
      let withBool = pure . MyLiteral mempty . MyBool
      if removeAnn plainA == removeAnn plainB
        then withBool True
        else withBool False
    Add -> do
      let withInt = pure . MyLiteral mempty . MyInt
      let getNum exp' = case exp' of
            (MyLiteral _ (MyInt i)) -> Right i
            _ -> Left $ AdditionWithNonNumber a
      case (,) <$> getNum plainA <*> getNum plainB of
        Right (a', b') -> withInt (a' + b')
        Left e -> throwError e
    Subtract -> do
      let withInt = pure . MyLiteral mempty . MyInt
      let getNum exp' = case exp' of
            (MyLiteral _ (MyInt i)) -> Right i
            _ -> Left $ SubtractionWithNonNumber a
      case (,) <$> getNum plainA <*> getNum plainB of
        Right (a', b') -> withInt (a' - b')
        Left e -> throwError e
    StringConcat ->
      interpretStringConcat plainA plainB
    ArrayConcat ->
      interpretArrayConcat plainA plainB
    (Custom infixOp) -> do
      opFn <- findOperator infixOp
      case opFn of
        Just fn -> do
          iFn <- interpretWithScope fn
          interpretWithScope
            ( MyApp
                mempty
                (MyApp mempty iFn plainA)
                plainB
            )
        Nothing ->
          throwError (CouldNotFindInfixOp infixOp)

interpretApplication ::
  (Eq ann, Monoid ann) =>
  ann ->
  Expr Variable ann ->
  Expr Variable ann ->
  App ann (Expr Variable ann)
interpretApplication ann fn value = do
  incrementApplyCount
  case fn of
    (MyVar ann' f) -> do
      expr <- interpretWithScope (MyVar ann' f)
      interpretWithScope (MyApp ann expr value)
    (MyLambda _ binder expr) -> do
      value' <- interpretWithScope value
      addToScope (Scope $ M.singleton binder value')
      interpretWithScope expr
    (MyLiteral ann' a) ->
      throwError $ CannotApplyToNonFunction (MyLiteral ann' a)
    (MyApp ann' f a) -> do
      inner <- interpretApplication ann' f a
      value' <- interpretWithScope value
      if inner == MyApp ann' f a && value == value'
        then pure (MyApp ann inner value')
        else interpretWithScope (MyApp ann inner value')
    (MyConstructor ann' const') ->
      MyApp ann (MyConstructor ann' const')
        <$> interpretWithScope value
    other -> do
      expr <- interpretWithScope other
      interpretWithScope (MyApp ann expr value)

interpretRecordAccess ::
  (Eq ann, Monoid ann) =>
  ann ->
  Expr Variable ann ->
  Name ->
  App ann (Expr Variable ann)
interpretRecordAccess ann recordExpr name =
  case recordExpr of
    (MyRecord _ record) ->
      case M.lookup name record of
        Just item -> interpretWithScope item
        _ -> throwError $ CannotFindMemberInRecord record name
    (MyVar ann' a) -> do
      expr <- interpretWithScope (MyVar ann' a)
      interpretWithScope (MyRecordAccess ann expr name)
    (MyRecordAccess ann' a name') -> do
      expr <- interpretWithScope (MyRecordAccess ann' a name')
      interpretWithScope (MyRecordAccess ann expr name)
    a ->
      throwError $ CannotDestructureAsRecord a name

interpretIf ::
  (Eq ann, Monoid ann) =>
  ann ->
  Expr Variable ann ->
  Expr Variable ann ->
  Expr Variable ann ->
  App ann (Expr Variable ann)
interpretIf ann predicate true false =
  case predicate of
    (MyLiteral _ (MyBool pred')) ->
      if pred'
        then interpretWithScope true
        else interpretWithScope false
    all'@MyLiteral {} ->
      throwError $ PredicateForIfMustBeABoolean all'
    all'@MyLambda {} ->
      throwError $ PredicateForIfMustBeABoolean all'
    pred' -> do
      predExpr <- interpretWithScope pred'
      interpretWithScope (MyIf ann predExpr true false)

interpretLetPattern ::
  (Eq ann, Monoid ann) =>
  Pattern Variable ann ->
  Expr Variable ann ->
  Expr Variable ann ->
  App ann (Expr Variable ann)
interpretLetPattern pat expr body = do
  let matches = fromMaybe [] (patternMatches pat expr)
  let newScopes = Scope $ M.fromList matches
  addToScope newScopes
  interpretWithScope body

-- warning, ordering is very important here to stop things looping forever
interpretWithScope ::
  (Eq ann, Monoid ann) =>
  Expr Variable ann ->
  App ann (Expr Variable ann)
interpretWithScope interpretExpr =
  case interpretExpr of
    (MyLet _ binder expr body) -> do
      addToScope (Scope $ M.singleton binder expr)
      interpretWithScope body
    (MyLetPattern _ pat expr body) -> do
      expr' <- interpretWithScope expr
      interpretLetPattern pat expr' body
    (MyInfix _ op a b) -> interpretOperator op a b
    (MyVar _ var) ->
      useVar var >>= interpretWithScope
    (MyApp ann fn value) -> interpretApplication ann fn value
    (MyRecordAccess ann recordExpr name) ->
      interpretRecordAccess ann recordExpr name
    (MyRecord ann as) -> do
      exprs <- traverse interpretWithScope as
      pure (MyRecord ann exprs)
    (MyLambda ann a b) ->
      pure (MyLambda ann a b)
    (MyIf ann predicate true false) ->
      interpretIf ann predicate true false
    (MyData _ _ expr) -> interpretWithScope expr
    (MyPatternMatch _ expr' patterns) -> do
      expr'' <- interpretWithScope expr'
      patternMatch expr'' patterns >>= interpretWithScope
    (MyDefineInfix _ op fn expr) -> do
      addOperator op fn
      interpretWithScope expr
    typedHole@MyTypedHole {} -> throwError (TypedHoleFound typedHole)
    expr -> bindExpr interpretWithScope expr
