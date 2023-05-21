{-# LANGUAGE OverloadedStrings #-}

module Test.Interpreter.InterpreterSpec (spec) where

import Calc
import Control.Monad (void)
import Control.Monad.Identity
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Text (Text)
import Test.Hspec

-- | try parsing the input, exploding if it's invalid
unsafeParseExpr :: Text -> Expr ()
unsafeParseExpr input = case parseExprAndFormatError input of
  Right a -> a $> ()
  Left e -> error (show e)

-- | function for testing the interpreter
testInterpret :: Text -> Expr ()
testInterpret =
  void -- throw away the `Annotation`s and replace with `()`
    . runIdentity -- unwrap the Identity Monad
    . interpret -- run the actual function
    . unsafeParseExpr -- parse the input (and explode if it's invalid)

spec :: Spec
spec = do
  describe "InterpreterSpec" $ do
    let cases =
          [ ("1 + 1", "2"),
            ("-11 + 1", "-10"),
            ("3 * 3 + 1", "10"),
            ("(3 * 3) + (6 * 6)", "45")
          ]
    traverse_
      ( \(input, expect) ->
          it (show input <> " = " <> show expect) $ do
            testInterpret input
              `shouldBe` unsafeParseExpr expect
      )
      cases
