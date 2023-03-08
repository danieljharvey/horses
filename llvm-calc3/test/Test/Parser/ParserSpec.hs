{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.ParserSpec (spec) where

import Calc
import Data.Foldable (traverse_)
import Data.Functor
import Data.String
import qualified Data.Text as T
import Test.Hspec

int :: (Monoid ann) => Integer -> Expr ann
int = EPrim mempty . PInt

bool :: (Monoid ann) => Bool -> Expr ann
bool = EPrim mempty . PBool

var :: (Monoid ann) => String -> Expr ann
var = EVar mempty . Identifier . fromString

spec :: Spec
spec = do
  describe "ParserSpec" $ do
    describe "Type" $ do
      let strings =
            [ ("Boolean", TPrim () TBool),
              ("Integer", TPrim () TInt)
            ]
      traverse_
        ( \(str, expr) -> it (T.unpack str) $ do
            case parseTypeAndFormatError str of
              Right parsedExp -> parsedExp $> () `shouldBe` expr
              Left e -> error (T.unpack e)
        )
        strings

    describe "Function" $ do
      let strings =
            [ ("function one() { 1 }", Function () [] "one" (int 1)),
              ( "function sum (a: Integer, b: Integer) { a + b }",
                Function ()
                  [("a", TPrim () TInt), ("b", TPrim () TInt)]
                  "sum"
                  ( EInfix () OpAdd (var "a") (var "b")
                  )
              )
            ]
      traverse_
        ( \(str, fn) -> it (T.unpack str) $ do
            case parseFunctionAndFormatError str of
              Right parsedFn -> parsedFn $> () `shouldBe` fn
              Left e -> error (T.unpack e)
        )
        strings

    describe "Expr" $ do
      let strings =
            [ ("-1", int (-1)),
              ("1 + 2", EInfix () OpAdd (int 1) (int 2)),
              ("True", EPrim () (PBool True)),
              ("False", EPrim () (PBool False)),
              ( "1 + 2 + 3",
                EInfix
                  ()
                  OpAdd
                  ( EInfix
                      ()
                      OpAdd
                      (int 1)
                      (int 2)
                  )
                  (int 3)
              ),
              ("1 == 2", EInfix () OpEquals (int 1) (int 2)),
              ("if True then 1 else 2", EIf () (bool True) (int 1) (int 2)),
              ("a + 1", EInfix () OpAdd (var "a") (int 1))
            ]
      traverse_
        ( \(str, expr) -> it (T.unpack str) $ do
            case parseExprAndFormatError str of
              Right parsedExp -> parsedExp $> () `shouldBe` expr
              Left e -> error (T.unpack e)
        )
        strings

    describe "Expr with Annotation" $ do
      it "Parses an infix operation with annotations" $ do
        parseExprAndFormatError "20 + 22"
          `shouldBe` Right
            ( EInfix
                (Location 0 7)
                OpAdd
                (EPrim (Location 0 2) (PInt 20))
                (EPrim (Location 5 7) (PInt 22))
            )
