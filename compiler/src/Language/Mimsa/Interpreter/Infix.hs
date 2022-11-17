module Language.Mimsa.Interpreter.Infix (interpretInfix) where

import Control.Monad.Except
import Language.Mimsa.Core
import Language.Mimsa.Interpreter.Monad
import Language.Mimsa.Interpreter.SimpleExpr
import Language.Mimsa.Interpreter.ToHOAS
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST.HOASExpr

import Debug.Trace
import Language.Mimsa.Types.Error.InterpreterError

-- | this assumes that
interpretInfix ::
  (Monoid ann) =>
  InterpretFn ann ->
  Operator ->
  InterpretExpr ann ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretInfix interpretFn operator a b = do
  plainA <- interpretFn <=< interpretFn $ a
  plainB <- interpretFn <=< interpretFn $ b
  case operator of
    Equals -> do
      let withBool = pure . MyLiteral mempty . MyBool
      if traceShowId (simpleExpr (fromHOAS plainA)) == traceShowId (simpleExpr (fromHOAS plainB))
        then withBool True
        else withBool False
    Add -> do
      let withInt = pure . MyLiteral mempty . MyInt
      let getNum exp' = case exp' of
            (MyLiteral _ (MyInt i)) -> Right i
            _ -> Left $ AdditionWithNonNumber (fromHOAS a)
      case (,) <$> getNum plainA <*> getNum plainB of
        Right (a', b') -> withInt (a' + b')
        Left e -> throwError e
    Subtract -> do
      let withInt = pure . MyLiteral mempty . MyInt
      let getNum exp' = case exp' of
            (MyLiteral _ (MyInt i)) -> Right i
            _ -> Left $ SubtractionWithNonNumber (fromHOAS exp')
      case (,) <$> getNum plainA <*> getNum plainB of
        Right (a', b') -> withInt (a' - b')
        Left e -> throwError e
    GreaterThan ->
      numericComparison
        (>)
        (ComparisonWithNonNumber GreaterThan . fromHOAS)
        plainA
        plainB
    GreaterThanOrEqualTo ->
      numericComparison
        (>=)
        (ComparisonWithNonNumber GreaterThanOrEqualTo . fromHOAS)
        plainA
        plainB
    LessThan ->
      numericComparison
        (<)
        (ComparisonWithNonNumber LessThan . fromHOAS)
        plainA
        plainB
    LessThanOrEqualTo ->
      numericComparison
        (<=)
        (ComparisonWithNonNumber LessThanOrEqualTo . fromHOAS)
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
  (Monoid ann) =>
  (Int -> Int -> Bool) ->
  (InterpretExpr ann -> InterpreterError Name ann) ->
  InterpretExpr ann ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
numericComparison f withErr plainA plainB = do
  let withBool = pure . MyLiteral mempty . MyBool
  let getNum exp' = case exp' of
        (MyLiteral _ (MyInt i)) -> Right i
        _ -> Left $ withErr exp'
  case (,) <$> getNum plainA <*> getNum plainB of
    Right (a', b') -> withBool (f a' b')
    Left e -> throwError e

interpretStringConcat ::
  (Monoid ann) =>
  InterpretExpr ann ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretStringConcat plainA plainB = do
  let withStr = pure . MyLiteral mempty . MyString . StringType
      getStr exp' = case exp' of
        (MyLiteral _ (MyString (StringType i))) -> Right i
        _ -> Left $ StringConcatenationFailure (fromHOAS plainA) (fromHOAS plainB)
  case (,) <$> getStr plainA <*> getStr plainB of
    Right (a', b') -> withStr (a' <> b')
    Left e -> throwError e

interpretArrayConcat ::
  (Monoid ann) =>
  InterpretExpr ann ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretArrayConcat plainA plainB = do
  let withArr = pure . MyArray mempty
      getArr exp' = case exp' of
        (MyArray _ i) -> Right i
        _ -> Left $ ArrayConcatenationFailure (fromHOAS plainA) (fromHOAS plainB)
  case (,) <$> getArr plainA <*> getArr plainB of
    Right (a', b') -> withArr (a' <> b')
    Left e -> throwError e
