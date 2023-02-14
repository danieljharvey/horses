{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.ParserSpec (spec) where

import Calc
import Calc.Types.Type
import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.Text as T
import Test.Hspec

int :: (Monoid ann) => Integer -> Expr ann
int = EPrim mempty . PInt

bool :: (Monoid ann) => Bool -> Expr ann
bool = EPrim mempty . PBool

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
              ("if True then 1 else 2", EIf () (bool True) (int 1) (int 2))
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
