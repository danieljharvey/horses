{-# LANGUAGE OverloadedStrings #-}

module Test.Syntax
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Parser
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Test.Hspec
import Test.Utils.Helpers
import Text.Megaparsec

-- specialisation of parseExpr
testParse :: Text -> Either String (Expr Name ())
testParse t = case parseExpr t of
  Right expr -> pure (toEmptyAnnotation expr)
  Left e -> Left $ errorBundlePretty e

testParseWithAnn :: Text -> Either String (Expr Name Annotation)
testParseWithAnn t = case parseExpr t of
  Right expr -> pure expr
  Left e -> Left $ errorBundlePretty e

spec :: Spec
spec = do
  describe "Language" $ do
    it "Parses True" $
      testParse "True" `shouldBe` Right (bool True)
    it "Parses False" $
      testParse "False" `shouldBe` Right (bool False)
    it "Parses 6" $
      testParse "6" `shouldBe` Right (int 6)
    it "Parses 1234567" $
      testParse "1234567" `shouldBe` Right (int 1234567)
    it "Does not parse 123.0" $
      testParse "123.0" `shouldSatisfy` isLeft
    it "Does not parse 123 with a space at the end" $
      testParse "123 " `shouldSatisfy` isLeft
    it "Does not parse literal with a space at the end" $
      testParse "True " `shouldSatisfy` isLeft
    it "Parses -6" $
      testParse "-6" `shouldBe` Right (int (-6))
    it "Parses +6" $
      testParse "+6" `shouldBe` Right (int 6)
    it "Parses a string" $
      testParse "\"dog\"" `shouldBe` Right (str (StringType "dog"))
    it "Parses a variable name" $
      testParse "log"
        `shouldBe` Right (MyVar mempty (mkName "log"))
    it "Does not accept 'let' as a variable name" $
      isLeft (testParse "let")
        `shouldBe` True
    it "Does not accept 'in' as a variable name" $
      isLeft (testParse "in")
        `shouldBe` True
    it "Does not accept 2log as a variable name because it starts with a number" $
      testParse "2log" `shouldSatisfy` isLeft
    it "Does not recognise a stupid variable name with crap in it" $
      testParse "log!dog"
        `shouldSatisfy` isLeft
    it "Does a basic let binding" $ do
      let expected = MyLet mempty (mkName "xa") (bool True) (MyVar mempty (mkName "xa"))
      testParse "let xa = True in xa"
        `shouldBe` Right expected
    it "Does a basic let binding with excessive whitespace" $ do
      let expected = MyLet mempty (mkName "x") (bool True) (MyVar mempty (mkName "x"))
      testParse "let       x       =       True       in        x"
        `shouldBe` Right expected
    it "Does a let binding inside parens" $ do
      let expected = MyLet mempty (mkName "x") (bool True) (MyVar mempty (mkName "x"))
      testParse "(let x = True in x)"
        `shouldBe` Right expected
    it "Recognises a basic lambda" $
      testParse "\\x -> x"
        `shouldBe` Right (MyLambda mempty (mkName "x") (MyVar mempty (mkName "x")))
    it "Recognises a lambda with too much whitespace everywhere" $
      testParse "\\        x          ->             x"
        `shouldBe` Right (MyLambda mempty (mkName "x") (MyVar mempty (mkName "x")))
    it "Recognises a lambda in parens" $
      testParse "(\\x -> x)"
        `shouldBe` Right (MyLambda mempty (mkName "x") (MyVar mempty (mkName "x")))
    it "Recognises nested lambdas in parens" $
      testParse "(\\a -> (\\b -> a))"
        `shouldBe` Right
          ( MyLambda
              mempty
              (mkName "a")
              (MyLambda mempty (mkName "b") (MyVar mempty (mkName "a")))
          )
    it "Recognises function application in parens" $
      testParse "add (1)"
        `shouldBe` Right
          ( MyApp
              mempty
              ( MyVar mempty (mkName "add")
              )
              (int 1)
          )
    it "Recognises double function application onto a var" $
      testParse "add (1)(2)"
        `shouldBe` Right
          ( MyApp
              mempty
              ( MyApp
                  mempty
                  ( MyVar mempty (mkName "add")
                  )
                  (int 1)
              )
              (int 2)
          )
    it "Recognises an if statement" $ do
      let expected = MyIf mempty (bool True) (int 1) (int 2)
      testParse "if True then 1 else 2" `shouldBe` Right expected
    it "Recognises an if statement in parens" $ do
      let expected = MyIf mempty (bool True) (int 1) (int 2)
      testParse "(if True then 1 else 2)" `shouldBe` Right expected
    it "Recognises an if statement with lots of whitespace" $ do
      let expected = MyIf mempty (bool True) (int 1) (int 2)
      testParse "if   True    then    1    else    2" `shouldBe` Right expected
    it "Parses a pair of things" $
      testParse "(2, 2)"
        `shouldBe` Right
          (MyPair mempty (int 2) (int 2))
    it "Parses a pair of things with silly whitespace" $
      testParse "(     2    ,   2     )"
        `shouldBe` Right
          (MyPair mempty (int 2) (int 2))
    it "Allows a let to use a pair" $
      testParse "let x = ((1,2)) in x"
        `shouldBe` Right
          ( MyLet
              mempty
              (mkName "x")
              (MyPair mempty (int 1) (int 2))
              (MyVar mempty (mkName "x"))
          )
    it "Allows a let to use a pair and apply to it" $
      testParse "let x = ((1,2)) in fst(x)"
        `shouldBe` Right
          ( MyLet
              mempty
              (mkName "x")
              (MyPair mempty (int 1) (int 2))
              (MyApp mempty (MyVar mempty (mkName "fst")) (MyVar mempty (mkName "x")))
          )
    it "Allows a let to use a nested lambda" $
      testParse "let const2 = (\\a -> (\\b -> a)) in (const2)"
        `shouldBe` Right
          ( MyLet
              mempty
              (mkName "const2")
              ( MyLambda
                  mempty
                  (mkName "a")
                  (MyLambda mempty (mkName "b") (MyVar mempty (mkName "a")))
              )
              (MyVar mempty (mkName "const2"))
          )
    it "Parses typed hole" $ do
      testParse "?dog" `shouldBe` Right (MyTypedHole mempty (mkName "dog"))
    it "Parses a complex let expression" $
      testParse "let const2 = (\\a -> (\\b -> a)) in (let reuse = ({first: const2(True), second: const2(2)}) in reuse.second(100))"
        `shouldSatisfy` isRight
    it "Parses an infix equals expression" $
      testParse "True == True" `shouldBe` Right (MyInfix mempty Equals (bool True) (bool True))
    it "Parses two integers with infix operator" $
      testParse "123 == 123" `shouldBe` Right (MyInfix mempty Equals (int 123) (int 123))
    it "Parses var and number equality" $
      testParse "a == 1" `shouldBe` Right (MyInfix mempty Equals (MyVar mempty (mkName "a")) (int 1))
    it "Parsers two constructor applications with infix operator" $
      let mkSome = MyConsApp mempty (MyConstructor mempty (mkTyCon "Some"))
       in testParse "(Some 1) == Some 2"
            `shouldBe` Right (MyInfix mempty Equals (mkSome (int 1)) (mkSome (int 2)))
    it "Parses an empty record literal" $
      testParse "{}" `shouldBe` Right (MyRecord mempty mempty)
    it "Parses a record literal with a single item inside" $
      testParse "{ dog: 1 }"
        `shouldBe` Right
          (MyRecord mempty (M.singleton (mkName "dog") (int 1)))
    it "Parses a record literal with multiple items inside" $
      testParse "{ dog:1, cat:True, horse:\"of course\" }"
        `shouldBe` Right
          ( MyRecord mempty $
              M.fromList
                [ (mkName "dog", int 1),
                  (mkName "cat", bool True),
                  (mkName "horse", str' "of course")
                ]
          )
    it "Parses a record literal with multiple items inside and less spacing" $
      testParse "{dog:1,cat:True,horse:\"of course\"}"
        `shouldBe` Right
          ( MyRecord mempty $
              M.fromList
                [ (mkName "dog", int 1),
                  (mkName "cat", bool True),
                  (mkName "horse", str' "of course")
                ]
          )
    it "Parses a destructuring of pairs" $
      testParse "let (a,b) = ((True,1)) in a"
        `shouldBe` Right
          ( MyLetPair
              mempty
              (mkName "a")
              (mkName "b")
              (MyPair mempty (bool True) (int 1))
              (MyVar mempty (mkName "a"))
          )
    it "Parses a destructuring of pairs with silly whitespace" $
      testParse "let   (    a ,      b ) =    ((       True, 1) ) in a"
        `shouldBe` Right
          ( MyLetPair
              mempty
              (mkName "a")
              (mkName "b")
              (MyPair mempty (bool True) (int 1))
              (MyVar mempty (mkName "a"))
          )
    it "Parses Void" $
      testParse "type Void in 1"
        `shouldBe` Right
          ( MyData
              mempty
              ( DataType
                  (mkTyCon "Void")
                  mempty
                  mempty
              )
              (int 1)
          )
    it "Parses an absolute unit" $
      testParse "type AbsoluteUnit = AbsoluteUnit in 1"
        `shouldBe` Right
          ( MyData
              mempty
              ( DataType
                  (mkTyCon "AbsoluteUnit")
                  mempty
                  (M.singleton (mkTyCon "AbsoluteUnit") mempty)
              )
              (int 1)
          )
    it "Parses an absolute unit with type var" $
      testParse "type Arr a = Empty\n | Item a\nin 1"
        `shouldBe` Right
          ( MyData
              mempty
              ( DataType
                  (mkTyCon "Arr")
                  [mkName "a"]
                  ( M.fromList
                      [ (mkTyCon "Empty", mempty),
                        ( mkTyCon "Item",
                          [VarName $mkName "a"]
                        )
                      ]
                  )
              )
              (int 1)
          )
    it "case (Just 1) of Just (\\ a -> eq(100)(a)) | \nNothing False" $
      testParse "case (Just 1) of Just (\\ a -> eq(100)(a)) | \nNothing False"
        `shouldSatisfy` isRight
    it "Parses a single constructor with one arg" $
      testParse "type Dog = Dog String in 1"
        `shouldBe` Right
          ( MyData
              mempty
              ( DataType
                  (mkTyCon "Dog")
                  mempty
                  ( M.singleton
                      (mkTyCon "Dog")
                      [ConsName (mkTyCon "String") mempty]
                  )
              )
              (int 1)
          )
    it "Parses a french boolean" $
      testParse "type LeBool = Vrai | Faux in 1"
        `shouldBe` Right
          ( MyData
              mempty
              ( DataType
                  (mkTyCon "LeBool")
                  mempty
                  ( M.fromList
                      [ (mkTyCon "Vrai", []),
                        (mkTyCon "Faux", [])
                      ]
                  )
              )
              (int 1)
          )
    it "Parses a peano number data declaration" $
      testParse "type Nat = Zero | Succ Nat in 1"
        `shouldBe` Right
          ( MyData
              mempty
              ( DataType
                  (mkTyCon "Nat")
                  mempty
                  ( M.fromList
                      [ (mkTyCon "Zero", []),
                        (mkTyCon "Succ", [ConsName (mkTyCon "Nat") mempty])
                      ]
                  )
              )
              (int 1)
          )
    it "Parses a multiple argument constructor" $
      testParse "Dog \"hi\" \"dog\""
        `shouldBe` Right
          ( MyConsApp
              mempty
              ( MyConsApp
                  mempty
                  (MyConstructor mempty $ mkTyCon "Dog")
                  (str' "hi")
              )
              (str' "dog")
          )
    it "Parses a type declaration with variable" $
      testParse "type Maybe a = Just a | Nothing in Nothing"
        `shouldBe` Right
          ( MyData
              mempty
              ( DataType
                  (mkTyCon "Maybe")
                  [mkName "a"]
                  ( M.fromList
                      [ (mkTyCon "Just", [VarName $ mkName "a"]),
                        (mkTyCon "Nothing", [])
                      ]
                  )
              )
              (MyConstructor mempty $ mkTyCon "Nothing")
          )
    it "Uses a constructor" $
      testParse "Vrai" `shouldBe` Right (MyConstructor mempty (mkTyCon "Vrai"))
    it "Parses a custom case match" $
      testParse "case Just 1 of Just \\a -> a | Nothing 0"
        `shouldBe` Right
          ( MyCaseMatch
              mempty
              (MyConsApp mempty (MyConstructor mempty $ mkTyCon "Just") (int 1))
              ( NE.fromList
                  [ (mkTyCon "Just", MyLambda mempty (mkName "a") (MyVar mempty (mkName "a"))),
                    (mkTyCon "Nothing", int 0)
                  ]
              )
              Nothing
          )
    it "Parses a custom case match with fall through case" $
      testParse "case Just 1 of Just \\a -> a | otherwise 0"
        `shouldBe` Right
          ( MyCaseMatch
              mempty
              (MyConsApp mempty (MyConstructor mempty $ mkTyCon "Just") (int 1))
              ( NE.fromList
                  [ (mkTyCon "Just", MyLambda mempty (mkName "a") (MyVar mempty (mkName "a")))
                  ]
              )
              (Just $ int 0)
          )
    it "Parses complex type constructors" $
      testParse "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Leaf 1"
        `shouldBe` Right
          ( MyData
              mempty
              ( DataType
                  (mkTyCon "Tree")
                  [mkName "a"]
                  ( M.fromList
                      [ (mkTyCon "Leaf", [VarName $ mkName "a"]),
                        ( mkTyCon "Branch",
                          [ ConsName (mkTyCon "Tree") [VarName $ mkName "a"],
                            ConsName (mkTyCon "Tree") [VarName $ mkName "b"]
                          ]
                        )
                      ]
                  )
              )
              (MyConsApp mempty (MyConstructor mempty $ mkTyCon "Leaf") (int 1))
          )
    it "Parses even more complex type constructors" $
      testParse "type Tree a = Empty | Branch (Tree a) a (Tree a) in Branch (Empty) 1 (Empty)"
        `shouldBe` Right
          ( MyData
              mempty
              ( DataType
                  (mkTyCon "Tree")
                  [mkName "a"]
                  ( M.fromList
                      [ (mkTyCon "Empty", mempty),
                        ( mkTyCon "Branch",
                          [ ConsName (mkTyCon "Tree") [VarName $ mkName "a"],
                            VarName $ mkName "a",
                            ConsName (mkTyCon "Tree") [VarName $ mkName "a"]
                          ]
                        )
                      ]
                  )
              )
              ( MyConsApp
                  mempty
                  ( MyConsApp
                      mempty
                      ( MyConsApp
                          mempty
                          (MyConstructor mempty $ mkTyCon "Branch")
                          (MyConstructor mempty $ mkTyCon "Empty")
                      )
                      (int 1)
                  )
                  (MyConstructor mempty $ mkTyCon "Empty")
              )
          )
    it "Parses big function application" $
      testParse "thing(1)(2)(3)(4)(5)"
        `shouldBe` Right
          ( MyApp
              mempty
              ( MyApp
                  mempty
                  ( MyApp
                      mempty
                      ( MyApp
                          mempty
                          ( MyApp
                              mempty
                              (MyVar mempty (mkName "thing"))
                              (int 1)
                          )
                          (int 2)
                      )
                      (int 3)
                  )
                  (int 4)
              )
              (int 5)
          )
  describe "Test annotations" $ do
    it "Parses a var with location information" $
      testParseWithAnn "dog" `shouldBe` Right (MyVar (Location 0 3) (mkName "dog"))
    it "Parses a tyCon with location information" $
      testParseWithAnn "Log" `shouldBe` Right (MyConstructor (Location 0 3) (mkTyCon "Log"))
    it "Parses a true bool with location information" $
      testParseWithAnn "True" `shouldBe` Right (MyLiteral (Location 0 4) (MyBool True))
    it "Parses a false bool with location information" $
      testParseWithAnn "False" `shouldBe` Right (MyLiteral (Location 0 5) (MyBool False))
    it "Parses a unit with location information" $
      testParseWithAnn "Unit" `shouldBe` Right (MyLiteral (Location 0 4) $ MyUnit ())
    it "Parses an integer with location information" $
      testParseWithAnn "100" `shouldBe` Right (MyLiteral (Location 0 3) (MyInt 100))
    it "Parses a string literal with location information" $
      testParseWithAnn "\"horse\"" `shouldBe` Right (MyLiteral (Location 0 7) (MyString $ StringType "horse"))
    it "Parses record access with location information" $
      testParseWithAnn "dog.tail"
        `shouldBe` Right
          ( MyRecordAccess
              (Location 0 8)
              (MyVar (Location 0 3) (mkName "dog"))
              (mkName "tail")
          )
    it "Parses let-in with location information" $
      testParseWithAnn "let a = 1 in a"
        `shouldBe` Right
          ( MyLet
              (Location 0 14)
              (mkName "a")
              (MyLiteral (Location 8 9) (MyInt 1))
              (MyVar (Location 13 14) (mkName "a"))
          )
    it "Parses let-newline with location information" $
      testParseWithAnn "let a = 1; a"
        `shouldBe` Right
          ( MyLet
              (Location 0 12)
              (mkName "a")
              (MyLiteral (Location 8 9) (MyInt 1))
              (MyVar (Location 11 12) (mkName "a"))
          )
    it "Parses let pair with location information" $
      testParseWithAnn "let (a,b) = dog in a"
        `shouldBe` Right
          ( MyLetPair
              (Location 0 20)
              (mkName "a")
              (mkName "b")
              (MyVar (Location 12 15) (mkName "dog"))
              (MyVar (Location 19 20) (mkName "a"))
          )
    it "Parsers lambda with location information" $
      testParseWithAnn "\\a -> a"
        `shouldBe` Right
          (MyLambda (Location 0 7) (mkName "a") (MyVar (Location 6 7) (mkName "a")))
    it "Parses application with location information" $
      testParseWithAnn "a(1)"
        `shouldBe` Right
          ( MyApp
              (Location 0 4)
              (MyVar (Location 0 1) (mkName "a"))
              (MyLiteral (Location 2 3) (MyInt 1))
          )
    it "Parses record with location information" $
      testParseWithAnn "{ a: True }"
        `shouldBe` Right
          ( MyRecord
              (Location 0 11)
              ( M.singleton
                  (mkName "a")
                  (MyLiteral (Location 5 9) (MyBool True))
              )
          )
    it "Parsers if with location information" $
      testParseWithAnn "if True then 1 else 2"
        `shouldBe` Right
          ( MyIf
              (Location 0 21)
              (MyLiteral (Location 3 7) (MyBool True))
              (MyLiteral (Location 13 14) (MyInt 1))
              (MyLiteral (Location 20 21) (MyInt 2))
          )
    it "Parsers pair with location information" $
      testParseWithAnn "(1,2)"
        `shouldBe` Right
          ( MyPair
              (Location 0 5)
              (MyLiteral (Location 1 2) (MyInt 1))
              (MyLiteral (Location 3 4) (MyInt 2))
          )
    it "Parses data declaration with location information" $
      testParseWithAnn "type MyUnit = MyUnit in 1"
        `shouldBe` Right
          ( MyData
              (Location 0 25)
              ( DataType
                  (mkTyCon "MyUnit")
                  mempty
                  (M.singleton (mkTyCon "MyUnit") mempty)
              )
              (MyLiteral (Location 24 25) (MyInt 1))
          )
    it "Parses constructor application with location information" $
      testParseWithAnn "Just 1"
        `shouldBe` Right
          ( MyConsApp
              (Location 0 6)
              (MyConstructor (Location 0 4) (mkTyCon "Just"))
              (MyLiteral (Location 5 6) (MyInt 1))
          )
    it "Parses case match with location information" $
      testParseWithAnn "case a of Just \\as -> 1 | Nothing 0"
        `shouldBe` Right
          ( MyCaseMatch
              (Location 0 35)
              (MyVar (Location 5 6) (mkName "a"))
              ( NE.fromList
                  [ ( mkTyCon "Just",
                      MyLambda
                        (Location 15 23)
                        (mkName "as")
                        (MyLiteral (Location 22 23) (MyInt 1))
                    ),
                    (mkTyCon "Nothing", MyLiteral (Location 34 35) (MyInt 0))
                  ]
              )
              Nothing
          )
    it "Parses infix equals with location information" $
      testParseWithAnn "1 == 2"
        `shouldBe` Right
          ( MyInfix
              (Location 0 6)
              Equals
              (MyLiteral (Location 0 1) (MyInt 1))
              (MyLiteral (Location 5 6) (MyInt 2))
          )
    it "Allows typed holes as function" $
      testParseWithAnn "\\a -> if ?tobool(a) then 1 else 2"
        `shouldSatisfy` isRight
