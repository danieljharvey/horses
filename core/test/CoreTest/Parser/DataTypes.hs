{-# LANGUAGE OverloadedStrings #-}

module CoreTest.Parser.DataTypes
  ( spec,
  )
where

import CoreTest.Utils.Helpers
import Data.Either (isRight)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Language.Mimsa.Core
import Test.Hspec
import Text.Megaparsec

-- specialisation of parseExpr
testParse :: Text -> Either String DataType
testParse t = case parseTypeDecl t of
  Right expr -> pure expr
  Left e -> Left $ errorBundlePretty e

spec :: Spec
spec = parallel $ do
  describe "DataTypes" $ do
    it "Parses Void" $
      testParse "type Void"
        `shouldBe` Right
          ( DataType
              "Void"
              mempty
              mempty
          )

    it "Parses an absolute unit" $
      testParse "type AbsoluteUnit = AbsoluteUnit"
        `shouldBe` Right
          ( DataType
              "AbsoluteUnit"
              mempty
              (M.singleton "AbsoluteUnit" mempty)
          )
    it "Parses an absolute unit with type var" $
      testParse "type Arr a = Item a"
        `shouldBe` Right
          ( DataType
              "Arr"
              ["a"]
              ( M.fromList
                  [ ( "Item",
                      [MTVar mempty (tvNamed "a")]
                    )
                  ]
              )
          )

    it "Parses an absolute unit with type var" $
      testParse "type Arr a = Empty\n | Item a"
        `shouldBe` Right
          ( DataType
              "Arr"
              ["a"]
              ( M.fromList
                  [ ("Empty", mempty),
                    ( "Item",
                      [MTVar mempty (tvNamed "a")]
                    )
                  ]
              )
          )

    it "Parses a single constructor with one arg" $
      testParse "type Dog = Dog String"
        `shouldBe` Right
          ( DataType
              "Dog"
              mempty
              ( M.singleton
                  "Dog"
                  [MTPrim mempty MTString]
              )
          )

    it "Parses a french boolean" $
      testParse "type LeBool = Vrai | Faux"
        `shouldBe` Right
          ( DataType
              "LeBool"
              mempty
              ( M.fromList
                  [ ("Vrai", []),
                    ("Faux", [])
                  ]
              )
          )

    it "Parses a peano number data declaration" $
      testParse "type Nat = Zero | Succ Nat"
        `shouldBe` Right
          ( DataType
              "Nat"
              mempty
              ( M.fromList
                  [ ("Zero", []),
                    ("Succ", [dataTypeWithVars mempty Nothing "Nat" mempty])
                  ]
              )
          )

    it "Parses a type declaration with variable" $
      testParse "type Maybe a = Just a | Nothing"
        `shouldBe` Right
          ( DataType
              "Maybe"
              ["a"]
              ( M.fromList
                  [ ("Just", [MTVar mempty (tvNamed "a")]),
                    ("Nothing", [])
                  ]
              )
          )

    it "Parses a type declaration with a function as arg" $
      testParse "type Reader r a = Reader (r -> a)"
        `shouldBe` Right
          ( DataType
              "Reader"
              ["r", "a"]
              ( M.fromList
                  [ ( "Reader",
                      [ MTFunction
                          mempty
                          (MTVar mempty (tvNamed "r"))
                          (MTVar mempty (tvNamed "a"))
                      ]
                    )
                  ]
              )
          )

    it "Parses a type declaration with a function and data type as arg" $
      testParse "type Reader r a = Reader (r -> (Pair a b))"
        `shouldBe` Right
          ( DataType
              "Reader"
              ["r", "a"]
              ( M.fromList
                  [ ( "Reader",
                      [ MTFunction
                          mempty
                          (MTVar mempty (tvNamed "r"))
                          ( dataTypeWithVars
                              mempty
                              Nothing
                              "Pair"
                              [ MTVar mempty (tvNamed "a"),
                                MTVar mempty (tvNamed "b")
                              ]
                          )
                      ]
                    )
                  ]
              )
          )

    it "Parses complex type constructors" $
      testParse "type Tree = Leaf Int | Branch Tree Tree"
        `shouldBe` Right
          ( DataType
              "Tree"
              []
              ( M.fromList
                  [ ("Leaf", [MTPrim mempty MTInt]),
                    ( "Branch",
                      [ dataTypeWithVars mempty Nothing "Tree" [],
                        dataTypeWithVars mempty Nothing "Tree" []
                      ]
                    )
                  ]
              )
          )
    it "Parses even more complex type constructors" $
      testParse "type Tree a = Empty | Branch (Tree a) a (Tree a)"
        `shouldBe` Right
          ( DataType
              "Tree"
              ["a"]
              ( M.fromList
                  [ ("Empty", mempty),
                    ( "Branch",
                      [ dataTypeWithVars mempty Nothing "Tree" [MTVar mempty (tvNamed "a")],
                        MTVar mempty (tvNamed "a"),
                        dataTypeWithVars mempty Nothing "Tree" [MTVar mempty (tvNamed "a")]
                      ]
                    )
                  ]
              )
          )

    it "Tree type" $
      testParse "type Tree a = Leaf a | Branch (Tree a) (Tree a)"
        `shouldBe` Right
          ( DataType
              "Tree"
              ["a"]
              ( M.fromList
                  [ ("Leaf", [MTVar mempty (tvNamed "a")]),
                    ( "Branch",
                      [ dataTypeWithVars mempty Nothing "Tree" [MTVar mempty (tvNamed "a")],
                        dataTypeWithVars mempty Nothing "Tree" [MTVar mempty (tvNamed "a")]
                      ]
                    )
                  ]
              )
          )

    it "Parses data declaration with location information" $
      testParse "type MyUnit = MyUnit"
        `shouldBe` Right
          ( DataType
              "MyUnit"
              mempty
              (M.singleton "MyUnit" mempty)
          )

    it "Parses Reader type declaration with 'in'" $
      testParse
        "type Reader r a = Reader (r -> a)"
        `shouldSatisfy` isRight

    it "Parses Reader type declaration with semicolon" $
      testParse
        "type Reader r a = Reader (r -> a)"
        `shouldSatisfy` isRight
