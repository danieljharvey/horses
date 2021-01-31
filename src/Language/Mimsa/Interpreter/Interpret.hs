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
    StringConcat -> do
      let withStr = pure . MyLiteral mempty . MyString . StringType
      let getStr exp' = case exp' of
            (MyLiteral _ (MyString (StringType i))) -> Right i
            _ -> Left $ ConcatentationWithNonString a
      case (,) <$> getStr plainA <*> getStr plainB of
        Right (a', b') -> withStr (a' <> b')
        Left e -> throwError e
    (Custom infixOp) -> do
      opFn <- findOperator infixOp
      case opFn of
        Just fnVar -> do
          fn <- useVar fnVar
          interpretWithScope (MyApp mempty (MyApp mempty fn plainA) plainB)
        Nothing ->
          throwError (CouldNotFindInfixOp infixOp)

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
    (MyLetPair _ binderA binderB (MyPair _ a b) body) -> do
      let newScopes = Scope $ M.fromList [(binderA, a), (binderB, b)]
      addToScope newScopes
      interpretWithScope body
    (MyLetPair ann binderA binderB (MyVar ann' v) body) -> do
      expr <- interpretWithScope (MyVar ann' v)
      interpretWithScope (MyLetPair ann binderA binderB expr body)
    (MyLetPair _ _ _ a _) ->
      throwError $ CannotDestructureAsPair a
    (MyInfix _ op a b) -> interpretOperator op a b
    (MyVar _ var) ->
      useVar var >>= interpretWithScope
    (MyApp ann (MyVar ann' f) value) -> do
      expr <- interpretWithScope (MyVar ann' f)
      interpretWithScope (MyApp ann expr value)
    (MyApp _ (MyLambda _ binder expr) value) -> do
      value' <- interpretWithScope value
      addToScope (Scope $ M.singleton binder value')
      interpretWithScope expr
    (MyApp _ (MyLiteral ann a) _) ->
      throwError $ CannotApplyToNonFunction (MyLiteral ann a)
    (MyApp ann other value) -> do
      expr <- interpretWithScope other
      interpretWithScope (MyApp ann expr value)
    (MyRecordAccess _ (MyRecord _ record) name) ->
      case M.lookup name record of
        Just item -> interpretWithScope item
        _ -> throwError $ CannotFindMemberInRecord record name
    (MyRecordAccess ann (MyVar ann' a) name) -> do
      expr <- interpretWithScope (MyVar ann' a)
      interpretWithScope (MyRecordAccess ann expr name)
    (MyRecordAccess ann (MyRecordAccess ann' a name') name) -> do
      expr <- interpretWithScope (MyRecordAccess ann' a name')
      interpretWithScope (MyRecordAccess ann expr name)
    (MyRecordAccess _ a name) ->
      throwError $ CannotDestructureAsRecord a name
    (MyRecord ann as) -> do
      exprs <- traverse interpretWithScope as
      pure (MyRecord ann exprs)
    (MyLambda ann a b) ->
      pure (MyLambda ann a b)
    (MyIf _ (MyLiteral _ (MyBool pred')) true false) ->
      if pred'
        then interpretWithScope true
        else interpretWithScope false
    (MyIf _ all'@(MyLiteral _ _) _ _) ->
      throwError $ PredicateForIfMustBeABoolean all'
    (MyIf _ all'@MyLambda {} _ _) ->
      throwError $ PredicateForIfMustBeABoolean all'
    (MyIf ann pred' true false) -> do
      predExpr <- interpretWithScope pred'
      interpretWithScope (MyIf ann predExpr true false)
    (MyData _ _ expr) -> interpretWithScope expr
    (MyCaseMatch _ expr' matches catchAll) -> do
      expr'' <- interpretWithScope expr'
      patternMatch expr'' matches catchAll >>= interpretWithScope
    (MyDefineInfix _ op fn expr) -> do
      addOperator op fn
      interpretWithScope expr
    typedHole@MyTypedHole {} -> throwError (TypedHoleFound typedHole)
    expr -> bindExpr interpretWithScope expr
