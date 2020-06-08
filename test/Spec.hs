{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Data.Aeson as JSON
import Data.Either (isLeft)
import Data.Text
import Language
import Lib
import qualified Parser as P
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances ()

instance Arbitrary Name where
  arbitrary = genericArbitrary

instance Arbitrary Expr where
  arbitrary = genericArbitrary

exprs :: [(Expr, Either Text MonoType)]
exprs =
  [ (MyInt 1, Right MTInt),
    (MyBool True, Right MTBool),
    ( MyString
        "hello",
      Right MTString
    ),
    (MyVar (Name "x"), Left "Unknown variable \"x\""),
    (MyLet (Name "x") (MyInt 42) (MyBool True), Right MTBool),
    (MyLet (Name "x") (MyInt 42) (MyVar (Name "x")), Right MTInt),
    ( MyLet
        (Name "x")
        (MyBool True)
        (MyLet (Name "y") (MyInt 42) (MyVar (Name "x"))),
      Right MTBool
    ),
    ( MyLet
        (Name "x")
        (MyBool True)
        (MyLet (Name "x") (MyInt 42) (MyVar (Name "x"))),
      Right MTInt
    ),
    ( MyLambda (Name "x") (MyBool True),
      Right $ MTFunction (MTUnknown (UniVar 1)) MTBool
    ),
    ( identity,
      Right $ MTFunction (MTUnknown (UniVar 1)) (MTUnknown (UniVar 1))
    ),
    ( MyLambda (Name "x") (MyLambda (Name "y") (MyVar (Name "x"))),
      Right $
        MTFunction
          (MTUnknown (UniVar 1))
          (MTFunction (MTUnknown (UniVar 2)) (MTUnknown (UniVar 1)))
    ),
    ( MyApp
        ( MyLambda
            (Name "x")
            (MyBool True)
        )
        (MyInt 1),
      Right MTBool
    ),
    ( MyApp
        identity
        (MyInt 1),
      Right MTInt
    ),
    ( MyApp
        ( MyLambda
            (Name "x")
            ( (MyIf (MyVar (Name "x")) (MyInt 10) (MyInt 10))
            )
        )
        (MyInt 100),
      Left "Can't match MTBool with MTInt"
    ),
    ( MyLambda (Name "x") (MyApp (MyVar (Name "x")) (MyVar (Name "x"))),
      Left "Cannot unify as MTUnknown 1 occurs within MTFunction (MTUnknown 1) (MTUnknown 2)"
    )
  ]

identity :: Expr
identity = (MyLambda (Name "x") (MyVar (Name "x")))

main :: IO ()
main = hspec $ do
  describe "Typechecker" $ do
    it "Our expressions typecheck as expected" $ do
      _ <- traverse (\(code, expected) -> startInference code `shouldBe` expected) exprs
      pure ()
    it "We can use identity with two different datatypes in one expression" $ do
      let lambda = (MyLambda (Name "x") (MyIf (MyApp identity (MyVar (Name "x"))) (MyApp identity (MyInt 1)) (MyApp identity (MyInt 2))))
      let expr = MyApp lambda (MyBool True)
      (startInference lambda) `shouldBe` Right (MTFunction (MTUnknown 1) MTInt)
      (startInference expr) `shouldBe` Right MTInt
    it "Serialisation" $ do
      property $ \x -> JSON.decode (JSON.encode x) == (Just x :: Maybe Expr)
  describe "Parser" $ do
    it "thenSpace match" $ do
      P.runParser (P.thenSpace (P.literal "let")) "let " `shouldBe` Right ("", "let")
    it "thenSpace mismatch" $ do
      isLeft (P.runParser (P.thenSpace (P.literal "let")) "let") `shouldBe` True
    it "applicative" $ do
      let parser = (,) <$> (P.literal "one") <*> (P.literal "two")
      P.runParser parser "onetwo" `shouldBe` Right ("", ("one", "two"))
    it "applicative with thenSpace" $ do
      let parser = (,) <$> (P.thenSpace (P.literal "one")) <*> (P.thenSpace (P.literal "two"))
      P.runParser parser "one      two " `shouldBe` Right ("", ("one", "two"))
  describe "Language" $ do
    it "Parses True" $ do
      parseExpr "True" `shouldBe` (Right (MyBool True))
    it "Parses False" $ do
      parseExpr "False" `shouldBe` (Right (MyBool False))
    it "Parses 6" $ do
      parseExpr "6" `shouldBe` (Right (MyInt 6))
    it "Parses 1234567" $ do
      parseExpr "1234567" `shouldBe` (Right (MyInt 1234567))
    it "Parses a string" $ do
      parseExpr "\"dog\"" `shouldBe` (Right (MyString "dog"))
    it "Parses a variable name" $ do
      parseExpr "log"
        `shouldBe` (Right (MyVar (Name "log")))
    it "Does not accept 'let' as a variable name" $ do
      isLeft (parseExpr "let")
        `shouldBe` True
    it "Does not accept 'in' as a variable name" $
      do
        isLeft (parseExpr "in")
        `shouldBe` True
    it "Does not accept 2log as a variable name because it starts with a number" $ do
      isLeft (parseExpr "2log") `shouldBe` True
    it "Does not recognise a stupid variable name with crap in it" $ do
      isLeft (parseExpr "log!dog")
        `shouldBe` True
    it "Does a basic let binding" $ do
      let expected = MyLet (Name "x") (MyBool True) (MyVar (Name "x"))
      parseExpr "let x = True in x"
        `shouldBe` Right expected
    it "Does a basic let binding with excessive whitespace" $ do
      let expected = MyLet (Name "x") (MyBool True) (MyVar (Name "x"))
      parseExpr "let       x       =       True       in        x"
        `shouldBe` Right expected
    it "Recognises a basic lambda" $ do
      parseExpr "\\x -> x"
        `shouldBe` Right (MyLambda (Name "x") (MyVar (Name "x")))
    it "Recognises a lambda with too much whitespace everywhere" $ do
      parseExpr "\\        x          ->             x"
        `shouldBe` Right (MyLambda (Name "x") (MyVar (Name "x")))
    it "Recognises function application onto a var" $ do
      parseExpr "add 1"
        `shouldBe` Right (MyApp (MyVar (Name "add")) (MyInt 1))
    it "Recognises an if statement" $ do
      let expected = MyIf (MyBool True) (MyInt 1) (MyInt 2)
      parseExpr' "if True then 1 else 2" `shouldBe` Right expected
    it "Recognises an if statement with lots of whitespace" $ do
      let expected = MyIf (MyBool True) (MyInt 1) (MyInt 2)
      parseExpr "if   True    then    1    else    2" `shouldBe` Right expected
