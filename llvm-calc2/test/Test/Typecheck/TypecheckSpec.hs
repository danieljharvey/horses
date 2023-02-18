{-# LANGUAGE OverloadedStrings #-}

module Test.Typecheck.TypecheckSpec (spec) where

import Calc.ExprUtils
import Calc.Parser
import Calc.Typecheck.Elaborate
import Calc.Typecheck.Error
import Calc.Types.Type
import Control.Monad
import Data.Foldable (traverse_)
import Data.Text (Text)
import Test.Hspec

testTypecheck :: (Text, Text) -> Spec
testTypecheck (input, result) = it (show input) $ do
  case (,) <$> parseExprAndFormatError input <*> parseTypeAndFormatError result of
    Left e -> error (show e)
    Right (expr, tyResult) -> do
      getOuterAnnotation <$> elaborate (void expr)
        `shouldBe` Right (void tyResult)

testFailing :: (Text, TypeError ()) -> Spec
testFailing (input, result) = it (show input) $ do
  case parseExprAndFormatError input of
    Left e -> error (show e)
    Right expr -> do
      getOuterAnnotation <$> elaborate (void expr)
        `shouldBe` Left result

spec :: Spec
spec = do
  describe "TypecheckSpec" $ do
    let succeeding =
          [ ("42", "Integer"),
            ("True", "Boolean"),
            ("1 + 1", "Integer"),
            ("6 * 9", "Integer"),
            ("1 - 10", "Integer"),
            ("2 == 2", "Boolean"),
            ("if True then 1 else 2", "Integer"),
            ("if False then True else False", "Boolean")
          ]

    describe "Successfully typechecking expressions" $ do
      traverse_ testTypecheck succeeding

    let failing =
          [ ("1 + True", TypeMismatch (TPrim () TInt) (TPrim () TBool)),
            ("if 1 then 1 else 2", PredicateIsNotBoolean ()  (TPrim () TInt)),
            ("if True then 1 else True", TypeMismatch (TPrim () TInt) (TPrim () TBool))
          ]

    describe "Failing typechecking expressions" $ do
      traverse_ testFailing failing
