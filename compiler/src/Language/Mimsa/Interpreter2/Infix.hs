module Language.Mimsa.Interpreter2.Infix (interpretInfix) where

import Control.Monad.Except
import Data.Functor
import Language.Mimsa.Interpreter2.Monad
import Language.Mimsa.Interpreter2.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError2

-- | this assumes that
interpretInfix ::
  (Ord var) =>
  InterpretFn var ann ->
  Operator ->
  InterpretExpr var ann ->
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretInfix interpretFn operator a b = do
  plainA <- interpretFn a
  plainB <- interpretFn b
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
            _ -> Left $ SubtractionWithNonNumber exp'
      case (,) <$> getNum plainA <*> getNum plainB of
        Right (a', b') -> withInt (a' - b')
        Left e -> throwError e
    GreaterThan ->
      numericComparison
        (>)
        (ComparisonWithNonNumber GreaterThan)
        plainA
        plainB
    GreaterThanOrEqualTo ->
      numericComparison
        (>=)
        (ComparisonWithNonNumber GreaterThanOrEqualTo)
        plainA
        plainB
    LessThan ->
      numericComparison
        (<)
        (ComparisonWithNonNumber LessThan)
        plainA
        plainB
    LessThanOrEqualTo ->
      numericComparison
        (<=)
        (ComparisonWithNonNumber LessThanOrEqualTo)
        plainA
        plainB
    StringConcat ->
      interpretStringConcat plainA plainB
    ArrayConcat ->
      interpretArrayConcat plainA plainB
    (Custom infixOp) -> do
      opFn <- findOperator infixOp
      iFn <- interpretFn opFn
      interpretFn
        ( MyApp
            mempty
            (MyApp mempty iFn plainA)
            plainB
        )

-- | lift a numeric comparison into the Expr type
numericComparison ::
  (Ord var) =>
  (Int -> Int -> Bool) ->
  (InterpretExpr var ann -> InterpreterError2 var ann) ->
  InterpretExpr var ann ->
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
numericComparison f withErr plainA plainB = do
  let withBool = pure . MyLiteral mempty . MyBool
  let getNum exp' = case exp' of
        (MyLiteral _ (MyInt i)) -> Right i
        _ -> Left $ withErr exp'
  case (,) <$> getNum plainA <*> getNum plainB of
    Right (a', b') -> withBool (f a' b')
    Left e -> throwError e

interpretStringConcat ::
  (Ord var) =>
  InterpretExpr var ann ->
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretStringConcat plainA plainB = do
  let withStr = pure . MyLiteral mempty . MyString . StringType
      getStr exp' = case exp' of
        (MyLiteral _ (MyString (StringType i))) -> Right i
        _ -> Left $ StringConcatenationFailure plainA plainB
  case (,) <$> getStr plainA <*> getStr plainB of
    Right (a', b') -> withStr (a' <> b')
    Left e -> throwError e

interpretArrayConcat ::
  (Ord var) =>
  InterpretExpr var ann ->
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretArrayConcat plainA plainB = do
  let withArr = pure . MyArray mempty
      getArr exp' = case exp' of
        (MyArray _ i) -> Right i
        _ -> Left $ ArrayConcatenationFailure plainA plainB
  case (,) <$> getArr plainA <*> getArr plainB of
    Right (a', b') -> withArr (a' <> b')
    Left e -> throwError e
