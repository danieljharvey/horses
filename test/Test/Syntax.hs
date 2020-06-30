{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Syntax
  ( spec,
  )
where

import qualified Data.Char as Ch
import Data.Either (isLeft, isRight)
import qualified Data.List.NonEmpty as NE
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

instance Arbitrary SumSide where
  arbitrary = genericArbitrary

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
      parseExpr "add (1)"
        `shouldBe` Right
          ( MyApp
              ( MyVar (mkName "add")
              )
              (int 1)
          )
    it "Recognises double function application onto a var" $ do
      parseExpr "add (1)(2)"
        `shouldBe` Right
          ( MyApp
              ( MyApp
                  ( MyVar (mkName "add")
                  )
                  (int 1)
              )
              (int 2)
          )
    it "Recognises an if statement" $ do
      let expected = MyIf (bool True) (int 1) (int 2)
      parseExpr' "if True then 1 else 2" `shouldBe` Right expected
    it "Recognises an if statement in parens" $ do
      let expected = MyIf (bool True) (int 1) (int 2)
      parseExpr' "(if True then 1 else 2)" `shouldBe` Right expected
    it "Recognises an if statement with lots of whitespace" $ do
      let expected = MyIf (bool True) (int 1) (int 2)
      parseExpr "if   True    then    1    else    2" `shouldBe` Right expected
    it "Parses a pair of things" $ do
      parseExpr "(2, 2)"
        `shouldBe` Right
          (MyPair (int 2) (int 2))
    it "Parses a pair of things with silly whitespace" $ do
      parseExpr "(     2    ,   2     )"
        `shouldBe` Right
          (MyPair (int 2) (int 2))
    it "Allows a let to use a pair" $ do
      parseExpr "let x = ((1,2)) in x"
        `shouldBe` Right
          ( MyLet
              (mkName "x")
              (MyPair (int 1) (int 2))
              (MyVar (mkName "x"))
          )
    it "Allows a let to use a pair and apply to it" $ do
      parseExpr "let x = ((1,2)) in fst(x)"
        `shouldBe` Right
          ( MyLet
              (mkName "x")
              (MyPair (int 1) (int 2))
              (MyApp (MyVar (mkName "fst")) (MyVar (mkName "x")))
          )
    it "Parses a list" $ do
      parseExpr "[1,2,3]"
        `shouldBe` Right (MyList (NE.fromList [int 1, int 2, int 3]))
    it "Parses a destructuring of a list" $ do
      parseExpr "let [head, rest] = ([1,2,3]) in head"
        `shouldBe` Right
          ( MyLetList
              (mkName "head")
              (mkName "rest")
              (MyList $ NE.fromList [int 1, int 2, int 3])
              (MyVar (mkName "head"))
          )
    it "Parses a destructuring of a list with a var" $ do
      parseExpr "let [head, rest] = myList in head"
        `shouldBe` Right
          ( MyLetList
              (mkName "head")
              (mkName "rest")
              (MyVar (mkName "myList"))
              (MyVar (mkName "head"))
          )
    it "Parses a destructuring of pairs" $ do
      parseExpr' "let (a,b) = ((True,1)) in a"
        `shouldBe` Right
          ( MyLetPair
              (mkName "a")
              (mkName "b")
              (MyPair (bool True) (int 1))
              (MyVar (mkName "a"))
          )
    it "Parses a destructuring of pairs with silly whitespace" $ do
      parseExpr' "let   (    a ,      b ) =    ((       True, 1) ) in a"
        `shouldBe` Right
          ( MyLetPair
              (mkName "a")
              (mkName "b")
              (MyPair (bool True) (int 1))
              (MyVar (mkName "a"))
          )
    it "Parses a case statement" $ do
      parseExpr "case horse of Left (\\l -> True) | Right (\\r -> False)"
        `shouldBe` Right
          ( MyCase
              (MyVar (mkName "horse"))
              (MyLambda (mkName "l") (bool True))
              (MyLambda (mkName "r") (bool False))
          )
    it "Parses a case statement with an apply in the sum" $ do
      parseExpr "case isTen(9) of Left (\\r -> \"It's not ten\") | Right (\\l -> \"It's ten!\")"
        `shouldBe` Right
          ( MyCase
              ( MyApp
                  (MyVar (mkName "isTen"))
                  (int 9)
              )
              (MyLambda (mkName "r") (str' "It's not ten"))
              (MyLambda (mkName "l") (str' "It's ten!"))
          )
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
