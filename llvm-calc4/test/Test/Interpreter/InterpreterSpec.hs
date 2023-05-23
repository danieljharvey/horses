{-# LANGUAGE OverloadedStrings #-}

module Test.Interpreter.InterpreterSpec (spec) where

import Calc
import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Text (Text)
import Test.Hspec

-- | try parsing the input, exploding if it's invalid
unsafeParseExpr :: Text -> Expr ()
unsafeParseExpr input = case parseExprAndFormatError input of
  Right a -> a $> ()
  Left e -> error (show e)

-- | try parsing the input, exploding if it's invalid
unsafeParseModule :: Text -> Module ()
unsafeParseModule input = case parseModuleAndFormatError input of
  Right a -> a $> ()
  Left e -> error (show e)

-- | function for testing the interpreter
testInterpret :: Text -> Either (InterpreterError ()) (Expr ())
testInterpret =
  fmap void -- throw away the `Annotation`s and replace with `()`
    . runInterpreter -- unwrap the InterpretM monad
    . interpret -- run the actual function
    . unsafeParseExpr -- parse the input (and explode if it's invalid)

-- | function for testing the interpreter
testInterpretModule :: Text -> Either (InterpreterError ()) (Expr ())
testInterpretModule =
  fmap void -- throw away the `Annotation`s and replace with `()`
    . runInterpreter -- unwrap the InterpretM monad
    . interpretModule -- run the actual function
    . unsafeParseModule -- parse the input (and explode if it's invalid)

spec :: Spec
spec = do
  describe "InterpreterSpec" $ do
    describe "Modules" $ do
      let cases =
            [ ("1 + 1", "2"),
              ("function increment(a: Integer) { a + 1 } increment(-11)", "-10"),
              ("function swap(pair: (Integer,Boolean)) { case pair of (a,b) -> (b,a) } swap(1,True)", "(True, 1)")
            ]
      traverse_
        ( \(input, expect) ->
            it (show input <> " = " <> show expect) $ do
              testInterpretModule input
                `shouldBe` Right (unsafeParseExpr expect)
        )
        cases
    describe "Expressions" $ do
      let cases =
            [ ("1 + 1", "2"),
              ("-11 + 1", "-10"),
              ("3 * 3 + 1", "10"),
              ("(3 * 3) + (6 * 6)", "45"),
              ("1 + 1 == 2", "True"),
              ("2 + 2 == 5", "False"),
              ("if False then True else False", "False"),
              ("if 1 == 1 then 6 else 5", "6"),
              ("case (1, True) of (a,False) -> a | (_,c) -> c", "True")
            ]
      traverse_
        ( \(input, expect) ->
            it (show input <> " = " <> show expect) $ do
              testInterpret input
                `shouldBe` Right (unsafeParseExpr expect)
        )
        cases
