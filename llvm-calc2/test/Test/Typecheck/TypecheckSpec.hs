{-# LANGUAGE OverloadedStrings #-}

module Test.Typecheck.TypecheckSpec (spec) where

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
      getOuterAnnotation <$> elaborate expr `shouldBe` Right tyResult

spec :: Spec
spec = do
  describe "TypecheckSpec" $ do
    let testVals =
          [ ("42", "Integer"),
            ("True", "Boolean")
          ]

    describe "Successfully typechecking expressions" $ do
      traverse_ testTypecheck testVals
