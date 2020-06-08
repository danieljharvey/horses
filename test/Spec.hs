{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

-- import qualified Data.Aeson as JSON
import qualified Data.Char as Ch
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language
import Lib
import qualified Parser as P
import Printer
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances ()
import Types (validName)

charListToText :: [Char] -> Text
charListToText = foldr T.cons ""

-- this is slow, there is probably a nicer way to do this
instance Arbitrary Name where
  arbitrary =
    let charList = charListToText <$> (listOf1 arbitraryASCIIChar)
     in mkName <$> suchThat charList validName

-- need to make these part of the contruction / parsing of the string type
-- or learn to unescape this crap
instance Arbitrary StringType where
  arbitrary =
    StringType
      <$> suchThat
        arbitrary
        (\a -> T.length a == T.length (T.filter isGoodChar a))
    where
      isGoodChar = Ch.isAlphaNum

instance Arbitrary Expr where
  arbitrary = genericArbitrary

newtype WellTypedExpr = WellTypedExpr Expr
  deriving (Show)

instance Arbitrary WellTypedExpr where
  arbitrary = WellTypedExpr <$> suchThat arbitrary (isRight . startInference)

exprs :: [(Expr, Either Text MonoType)]
exprs =
  [ (MyInt 1, Right MTInt),
    (MyBool True, Right MTBool),
    ( MyString
        (StringType "hello"),
      Right MTString
    ),
    (MyVar (mkName "x"), Left "Unknown variable \"x\""),
    (MyLet (mkName "x") (MyInt 42) (MyBool True), Right MTBool),
    (MyLet (mkName "x") (MyInt 42) (MyVar (mkName "x")), Right MTInt),
    ( MyLet
        (mkName "x")
        (MyBool True)
        (MyLet (mkName "y") (MyInt 42) (MyVar (mkName "x"))),
      Right MTBool
    ),
    ( MyLet
        (mkName "x")
        (MyBool True)
        (MyLet (mkName "x") (MyInt 42) (MyVar (mkName "x"))),
      Right MTInt
    ),
    ( MyLambda (mkName "x") (MyBool True),
      Right $ MTFunction (MTUnknown (UniVar 1)) MTBool
    ),
    ( identity,
      Right $ MTFunction (MTUnknown (UniVar 1)) (MTUnknown (UniVar 1))
    ),
    ( MyLambda (mkName "x") (MyLambda (mkName "y") (MyVar (mkName "x"))),
      Right $
        MTFunction
          (MTUnknown (UniVar 1))
          (MTFunction (MTUnknown (UniVar 2)) (MTUnknown (UniVar 1)))
    ),
    ( MyApp
        ( MyLambda
            (mkName "x")
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
            (mkName "x")
            ( (MyIf (MyVar (mkName "x")) (MyInt 10) (MyInt 10))
            )
        )
        (MyInt 100),
      Left "Can't match MTBool with MTInt"
    ),
    ( MyLambda (mkName "x") (MyApp (MyVar (mkName "x")) (MyVar (mkName "x"))),
      Left "Cannot unify as MTUnknown 1 occurs within MTFunction (MTUnknown 1) (MTUnknown 2)"
    )
  ]

identity :: Expr
identity = (MyLambda (mkName "x") (MyVar (mkName "x")))

main :: IO ()
main = hspec $ do
  describe "Typechecker" $ do
    it "Our expressions typecheck as expected" $ do
      _ <- traverse (\(code, expected) -> startInference code `shouldBe` expected) exprs
      pure ()
    it "We can use identity with two different datatypes in one expression" $ do
      let lambda = (MyLambda (mkName "x") (MyIf (MyApp identity (MyVar (mkName "x"))) (MyApp identity (MyInt 1)) (MyApp identity (MyInt 2))))
      let expr = MyApp lambda (MyBool True)
      (startInference lambda) `shouldBe` Right (MTFunction (MTUnknown 1) MTInt)
      (startInference expr) `shouldBe` Right MTInt
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
    it "Parses -6" $ do
      parseExpr "-6" `shouldBe` Right (MyInt (-6))
    it "Parses +6" $ do
      parseExpr "+6" `shouldBe` Right (MyInt 6)
    it "Parses a string" $ do
      parseExpr "\"dog\"" `shouldBe` (Right (MyString (StringType "dog")))
    it "Parses a variable name" $ do
      parseExpr "log"
        `shouldBe` (Right (MyVar (mkName "log")))
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
      let expected = MyLet (mkName "x") (MyBool True) (MyVar (mkName "x"))
      parseExpr "let x = True in x"
        `shouldBe` Right expected
    it "Does a basic let binding with excessive whitespace" $ do
      let expected = MyLet (mkName "x") (MyBool True) (MyVar (mkName "x"))
      parseExpr "let       x       =       True       in        x"
        `shouldBe` Right expected
    it "Does a let binding inside parens" $ do
      let expected = MyLet (mkName "x") (MyBool True) (MyVar (mkName "x"))
      parseExpr "(let x = True in x)"
        `shouldBe` Right expected
    it "Recognises a basic lambda" $ do
      parseExpr "\\x -> x"
        `shouldBe` Right (MyLambda (mkName "x") (MyVar (mkName "x")))
    it "Recognises a lambda with too much whitespace everywhere" $ do
      parseExpr "\\        x          ->             x"
        `shouldBe` Right (MyLambda (mkName "x") (MyVar (mkName "x")))
    it "Recognises a lambda in parens" $ do
      parseExpr "(\\x -> x)"
        `shouldBe` Right (MyLambda (mkName "x") (MyVar (mkName "x")))
    it "Recognises function application in parens" $ do
      parseExpr "(add 1)"
        `shouldBe` Right (MyApp (MyVar (mkName "add")) (MyInt 1))
    it "Recognises double function application onto a var" $ do
      parseExpr "((add 1) 2)"
        `shouldBe` Right (MyApp (MyApp (MyVar (mkName "add")) (MyInt 1)) (MyInt 2))
    it "Recognises an if statement" $ do
      let expected = MyIf (MyBool True) (MyInt 1) (MyInt 2)
      parseExpr' "if True then 1 else 2" `shouldBe` Right expected
    it "Recognises an if statement in parens" $ do
      let expected = MyIf (MyBool True) (MyInt 1) (MyInt 2)
      parseExpr' "(if True then 1 else 2)" `shouldBe` Right expected
    it "Recognises an if statement with lots of whitespace" $ do
      let expected = MyIf (MyBool True) (MyInt 1) (MyInt 2)
      parseExpr "if   True    then    1    else    2" `shouldBe` Right expected
  describe "Expression" $ do
    {-it "Serialisation" $ do
      property $ \x -> JSON.decode (JSON.encode x) == (Just x :: Maybe Expr)
    -}
    it "Printing and parsing is an iso" $ do
      property $ \(WellTypedExpr x) -> do
        case startInference x of
          Right type' -> do
            T.putStrLn ""
            T.putStrLn (prettyPrint type')
          _ -> pure ()
        T.putStrLn (prettyPrint x)
        parseExpr (prettyPrint x) `shouldBe` Right x
