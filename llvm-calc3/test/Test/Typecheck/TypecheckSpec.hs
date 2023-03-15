{-# LANGUAGE OverloadedStrings #-}

module Test.Typecheck.TypecheckSpec (spec) where

import Calc.ExprUtils
import Calc.Parser
import Calc.Typecheck.Elaborate
import Calc.Typecheck.Error
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Module
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

testSucceedingFunction :: (Text, Type ()) -> Spec
testSucceedingFunction (input, fn) =
  it (show input) $ do
    case parseFunctionAndFormatError input of
      Left e -> error (show e)
      Right parsedFn ->
        fnAnn <$> elaborateFunction (void parsedFn)
          `shouldBe` Right fn

testSucceedingModule :: (Text, Type ()) -> Spec
testSucceedingModule (input, md) =
  it (show input) $ do
    case parseModuleAndFormatError input of
      Left e -> error (show e)
      Right parsedMod ->
        getOuterAnnotation . mdExpr <$> elaborateModule (void parsedMod)
          `shouldBe` Right md

spec :: Spec
spec = do
  describe "TypecheckSpec" $ do
    describe "Function" $ do
      let succeeding =
            [ ("function one () { 1 }", TFunction () [] (TPrim () TInt)),
              ( "function not (bool: Boolean) { if bool then False else True }",
                TFunction () [TPrim () TBool] (TPrim () TBool)
              )
            ]

      describe "Successfully typechecking functions" $ do
        traverse_ testSucceedingFunction succeeding

    describe "Module" $ do
      let succeeding =
            [ ("function ignore() { 1 } 42", TPrim () TInt),
              ("function increment(a: Integer) { a + 1 } increment(41)", TPrim () TInt)
            ]
      describe "Successfully typechecking modules" $ do
        traverse_ testSucceedingModule succeeding

    describe "Expr" $ do
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
            [ ("if 1 then 1 else 2", PredicateIsNotBoolean () (TPrim () TInt)),
              ("if True then 1 else True", TypeMismatch (TPrim () TInt) (TPrim () TBool)),
              ("1 + True", InfixTypeMismatch OpAdd [(TPrim () TInt, TPrim () TBool)]),
              ("True + False", InfixTypeMismatch OpAdd [(TPrim () TInt, TPrim () TBool), (TPrim () TInt, TPrim () TBool)]),
              ("1 * False", InfixTypeMismatch OpMultiply [(TPrim () TInt, TPrim () TBool)]),
              ("True - 1", InfixTypeMismatch OpSubtract [(TPrim () TInt, TPrim () TBool)])
            ]

      describe "Failing typechecking expressions" $ do
        traverse_ testFailing failing
