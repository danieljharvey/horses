{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Syntax
  ( spec,
  )
where

-- import qualified Data.Aeson as JSON
import qualified Data.Char as Ch
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa
import Language.Mimsa.Syntax
import qualified Language.Mimsa.Syntax as P
import Language.Mimsa.Types
  ( FuncName,
    Literal,
    validName,
  )
import Test.Helpers
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances ()

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
        ( \a ->
            T.length a == T.length (T.filter isGoodChar a)
              && T.length a > 0
        )
    where
      isGoodChar = Ch.isAlphaNum

instance Arbitrary Expr where
  arbitrary = genericArbitrary

instance Arbitrary Literal where
  arbitrary = genericArbitrary

instance Arbitrary FuncName where
  arbitrary = genericArbitrary

newtype WellTypedExpr = WellTypedExpr Expr
  deriving (Show)

instance Arbitrary WellTypedExpr where
  arbitrary = WellTypedExpr <$> suchThat arbitrary (isRight . startInference)

spec :: Spec
spec = do
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
      parseExpr "True" `shouldBe` (Right (bool True))
    it "Parses False" $ do
      parseExpr "False" `shouldBe` (Right (bool False))
    it "Parses 6" $ do
      parseExpr "6" `shouldBe` (Right (int 6))
    it "Parses 1234567" $ do
      parseExpr "1234567" `shouldBe` (Right (int 1234567))
    it "Parses -6" $ do
      parseExpr "-6" `shouldBe` Right (int (-6))
    it "Parses +6" $ do
      parseExpr "+6" `shouldBe` Right (int 6)
    it "Parses a string" $ do
      parseExpr "\"dog\"" `shouldBe` (Right (str (StringType "dog")))
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
      let expected = MyLet (mkName "x") (bool True) (MyVar (mkName "x"))
      parseExpr "let x = True in x"
        `shouldBe` Right expected
    it "Does a basic let binding with excessive whitespace" $ do
      let expected = MyLet (mkName "x") (bool True) (MyVar (mkName "x"))
      parseExpr "let       x       =       True       in        x"
        `shouldBe` Right expected
    it "Does a let binding inside parens" $ do
      let expected = MyLet (mkName "x") (bool True) (MyVar (mkName "x"))
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
        `shouldBe` Right (MyApp (MyVar (mkName "add")) (int 1))
    it "Recognises double function application onto a var" $ do
      parseExpr "((add 1) 2)"
        `shouldBe` Right (MyApp (MyApp (MyVar (mkName "add")) (int 1)) (int 2))
    it "Recognises an if statement" $ do
      let expected = MyIf (bool True) (int 1) (int 2)
      parseExpr' "if True then 1 else 2" `shouldBe` Right expected
    it "Recognises an if statement in parens" $ do
      let expected = MyIf (bool True) (int 1) (int 2)
      parseExpr' "(if True then 1 else 2)" `shouldBe` Right expected
    it "Recognises an if statement with lots of whitespace" $ do
      let expected = MyIf (bool True) (int 1) (int 2)
      parseExpr "if   True    then    1    else    2" `shouldBe` Right expected
  describe "Expression" $ do
    it "Printing and parsing is an iso" $ do
      property $ \(WellTypedExpr x) -> do
        case startInference x of
          Right type' -> do
            T.putStrLn ""
            T.putStrLn (prettyPrint type')
          _ -> pure ()
        T.putStrLn (prettyPrint x)
        parseExpr (prettyPrint x) `shouldBe` Right x
