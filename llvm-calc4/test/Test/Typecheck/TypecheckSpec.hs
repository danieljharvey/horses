{-# LANGUAGE OverloadedStrings #-}

module Test.Typecheck.TypecheckSpec (spec) where

import Test.Helpers
import Calc.ExprUtils
import Calc.Parser
import Calc.Typecheck.Elaborate
import Calc.Typecheck.Error
import Calc.Typecheck.Types
import Calc.Types.Expr
import Calc.Types.Function
import Calc.Types.Module
import Calc.Types.Type
import Control.Monad
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Test.Hspec

runTC :: TypecheckM ann a -> Either (TypeError ann) a
runTC = runTypecheckM (TypecheckEnv mempty)

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
        fnAnn <$> runTC (elaborateFunction (void parsedFn))
          `shouldBe` Right fn

testSucceedingModule :: (Text, Type ()) -> Spec
testSucceedingModule (input, md) =
  it (show input) $ do
    case parseModuleAndFormatError input of
      Left e -> error (show e)
      Right parsedMod ->
        getOuterAnnotation . mdExpr <$> elaborateModule (void parsedMod)
          `shouldBe` Right md

testFailingModule :: Text -> Spec
testFailingModule input =
  it (show input) $ do
    case parseModuleAndFormatError input of
      Left e -> error (show e)
      Right parsedMod ->
        elaborateModule (void parsedMod)
          `shouldSatisfy` isLeft

spec :: Spec
spec = do
  describe "TypecheckSpec" $ do
    describe "Function" $ do
      let succeeding =
            [ ("function one () { 1 }", TFunction () [] tyInt),
              ( "function not (bool: Boolean) { if bool then False else True }",
                TFunction () [tyBool ] tyBool
              )
            ]

      describe "Successfully typechecking functions" $ do
        traverse_ testSucceedingFunction succeeding

    describe "Module" $ do
      let succeeding =
            [ ("function ignore() { 1 } 42", tyInt ),
              ("function increment(a: Integer) { a + 1 } increment(41)", tyInt ),
              ("function inc(a: Integer) { a + 1 } function inc2(a: Integer) { inc(a) } inc2(41)", TPrim () TInt)
            ]
      describe "Successfully typechecking modules" $ do
        traverse_ testSucceedingModule succeeding

      let failing =
            [ "function increment(b: Boolean) { a + 1 } increment(41)"
            ]
      describe "Failing typechecking modules" $ do
        traverse_ testFailingModule failing

    describe "Expr" $ do
      let succeeding =
            [ ("42", "Integer"),
              ("True", "Boolean"),
              ("1 + 1", "Integer"),
              ("6 * 9", "Integer"),
              ("1 - 10", "Integer"),
              ("2 == 2", "Boolean"),
              ("if True then 1 else 2", "Integer"),
              ("if False then True else False", "Boolean"),
              ("(1,2,True)", "(Integer,Integer,Boolean)"),
              ("case (1,2,3) of (a,b,_) -> a + b", "Integer"),
              ("case (1,True) of (2,b) -> b | _ -> False", "Boolean")
            ]

      describe "Successfully typechecking expressions" $ do
        traverse_ testTypecheck succeeding

      let failing =
            [ ("if 1 then 1 else 2", PredicateIsNotBoolean () tyInt ),
              ("if True then 1 else True", TypeMismatch tyInt tyBool),
              ("1 + True", InfixTypeMismatch OpAdd [(tyInt, tyBool)]),
              ("True + False", InfixTypeMismatch OpAdd [(tyInt, tyBool), (tyInt,tyBool)]),
              ("1 * False", InfixTypeMismatch OpMultiply [(TPrim () TInt, TPrim () TBool)]),
              ("True - 1", InfixTypeMismatch OpSubtract [(TPrim () TInt, TPrim () TBool)]),
              ("case (1,True) of (a, False) -> a | (_,c) -> c", TypeMismatch tyBool tyInt)
            ]

      describe "Failing typechecking expressions" $ do
        traverse_ testFailing failing
