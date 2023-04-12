{-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE LambdaCase #-}
module Test.Parser.ParserSpec (spec) where

import qualified Data.List.NonEmpty as NE
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

tuple :: (Monoid ann) => [Expr ann] -> Expr ann
tuple = \case
  (a : b : rest) -> ETuple mempty a (b NE.:| rest)
  _ -> error "not enough items for tuple"

tyInt :: (Monoid ann) => Type ann
tyInt = TPrim mempty TInt

tyBool :: (Monoid ann) => Type ann
tyBool = TPrim mempty TBool

tyTuple :: (Monoid ann) => [Type ann] -> Type ann
tyTuple = \case
  (a : b : rest) -> TTuple mempty a (b NE.:| rest)
  _ -> error "not enough items for tyTuple"

spec :: Spec
spec = do
  describe "ParserSpec" $ do
    describe "Type" $ do
      let strings =
            [ ("Boolean", tyBool),
              ("Integer", tyInt),
              ("(Boolean, Boolean, Integer)", tyTuple [tyBool, tyBool, tyInt])
            ]
      traverse_
        ( \(str, expr) -> it (T.unpack str) $ do
            case parseTypeAndFormatError str of
              Right parsedExp -> parsedExp $> () `shouldBe` expr
              Left e -> error (T.unpack e)
        )
        strings

    describe "Module" $ do
      let strings =
            [ ("42", Module [] (int 42)),
              ( "function increment(a: Integer) { a + 1 } 42",
                Module [Function () [("a", TPrim () TInt)] "increment" (EInfix () OpAdd (var "a") (int 1))] (int 42)
              ),
              ( "function increment(a: Integer) { a + 1 } function decrement(a: Integer) { a - 1} 42",
                Module
                  [ Function () [("a", TPrim () TInt)] "increment" (EInfix () OpAdd (var "a") (int 1)),
                    Function () [("a", TPrim () TInt)] "decrement" (EInfix () OpSubtract (var "a") (int 1))
                  ]
                  (int 42)
              )
            ]

      traverse_
        ( \(str, module') -> it (T.unpack str) $ do
            case parseModuleAndFormatError str of
              Right parsedMod -> parsedMod $> () `shouldBe` module'
              Left e -> error (T.unpack e)
        )
        strings

    describe "Function" $ do
      let strings =
            [ ("function one() { 1 }", Function () [] "one" (int 1)),
              ( "function sum (a: Integer, b: Integer) { a + b }",
                Function
                  ()
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
              ("(1,2,True)", tuple [int 1, int 2, bool True]),
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
              ("a + 1", EInfix () OpAdd (var "a") (int 1)),
              ("add(1,2)", EApply () "add" [int 1, int 2]),
              ("go()", EApply () "go" [])
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
