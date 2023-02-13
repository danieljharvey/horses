{-# LANGUAGE OverloadedStrings #-}

module Test.Typecheck.TypecheckSpec (spec) where

import Control.Monad
import Data.Foldable (traverse_)
import Test.Hspec
import Calc.Typecheck.Elaborate
import Calc.ExprUtils
import Data.Text (Text)
import Calc.Parser

testTypecheck :: (Text, Text) -> Spec
testTypecheck (input, result) = it (show input) $ do
  case (,) <$> parseExprAndFormatError input <*> parseTypeAndFormatError result of
    Left e -> error (show e)
    Right (expr,tyResult) -> do
      getOuterAnnotation <$> elaborate (void expr)
        `shouldBe` Right (void tyResult)

spec :: Spec
spec = do
  describe "TypecheckSpec" $ do
    let testVals =
          [ ("42", "Integer"),
            ("True", "Boolean"),
            ("1 + 1", "Integer"),
            ("6 * 9", "Integer"),
            ("1 - 10", "Integer"),
            ("2 == 2", "Boolean")
          ]

    describe "Successfully typechecking expressions" $ do
      traverse_ testTypecheck testVals
