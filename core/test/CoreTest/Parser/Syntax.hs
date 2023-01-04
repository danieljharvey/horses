{-# LANGUAGE OverloadedStrings #-}

module CoreTest.Parser.Syntax
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Text (Text)

import Language.Mimsa.Core

import CoreTest.Utils.Helpers

import qualified Data.Text as T

import Test.Hspec

-- remove annotations for comparison
toEmptyAnn :: Expr a b -> Expr a ()
toEmptyAnn = toEmptyAnnotation

-- does the output of our prettyprinting still make sense to the parser?
testParseWithAnn :: Text -> Either Text (Expr Name Annotation)
testParseWithAnn input = do
  expr1 <- parseExprAndFormatError input
  case parseExprAndFormatError (prettyPrint expr1) of
    Left e -> Left e
    Right expr2 ->
      if toEmptyAnn expr1 /= toEmptyAnn expr2
        then
          Left $
            "Pretty:\n"
              <> prettyPrint expr1
              <> "\nExpr:\n"
              <> T.pack (show expr1)
              <> "\nDoes not match re-parsed expr:\n"
              <> T.pack (show expr2)
        else pure expr1

-- specialisation of parseExpr
testParse :: Text -> Either Text (Expr Name ())
testParse t = toEmptyAnnotation <$> testParseWithAnn t

spec :: Spec
spec = parallel $ do
  describe "Syntax" $ do
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
      it "Parses 123 with a space at the end" $
        testParse "123 " `shouldSatisfy` isRight
      it "Parses literal with a space at the end" $
        testParse "True " `shouldSatisfy` isRight
      it "Parses -6" $
        testParse "-6" `shouldBe` Right (int (-6))
      it "Parses +6" $
        testParse "+6" `shouldBe` Right (int 6)
      it "Parses a string" $
        testParse "\"dog\"" `shouldBe` Right (str (StringType "dog"))
      it "Parses a variable name" $
        testParse "log"
          `shouldBe` Right (MyVar mempty Nothing "log")
      it "Parses a namespaced variable name" $
        testParse "Console.log"
          `shouldBe` Right (MyVar mempty (Just "Console") "log")
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
        let expected = MyLet mempty (Identifier mempty "xa") (bool True) (MyVar mempty Nothing "xa")
        testParse "let xa = True in xa"
          `shouldBe` Right expected
      it "Does a basic let binding with excessive whitespace" $ do
        let expected = MyLet mempty (Identifier mempty "x") (bool True) (MyVar mempty Nothing "x")
        testParse "let       x       =       True       in        x"
          `shouldBe` Right expected
      it "Does a let binding inside parens" $ do
        let expected = MyLet mempty (Identifier mempty "x") (bool True) (MyVar mempty Nothing "x")
        testParse "(let x = True in x)"
          `shouldBe` Right expected
      it "Recognises a basic lambda" $
        testParse "\\x -> x"
          `shouldBe` Right (MyLambda mempty (Identifier mempty "x") (MyVar mempty Nothing "x"))
      it "Recognises a lambda with too much whitespace everywhere" $
        testParse "\\        x          ->             x"
          `shouldBe` Right (MyLambda mempty (Identifier mempty "x") (MyVar mempty Nothing "x"))
      it "Recognises a lambda in parens" $
        testParse "(\\x -> x)"
          `shouldBe` Right (MyLambda mempty (Identifier mempty "x") (MyVar mempty Nothing "x"))
      it "Recognises nested lambdas in parens" $
        testParse "(\\a -> (\\b -> a))"
          `shouldBe` Right
            ( MyLambda
                mempty
                (Identifier mempty "a")
                (MyLambda mempty (Identifier mempty "b") (MyVar mempty Nothing "a"))
            )
      it "Recognises minimal function syntax in let" $
        testParse "let const a b = a in True"
          `shouldBe` Right
            ( MyLet
                mempty
                (Identifier mempty "const")
                (MyLambda mempty (Identifier mempty "a") (MyLambda mempty (Identifier mempty "b") (MyVar mempty Nothing "a")))
                (bool True)
            )
      it "Recognises function application" $
        testParse "add 1"
          `shouldBe` Right
            ( MyApp
                mempty
                ( MyVar mempty Nothing "add"
                )
                (int 1)
            )
      it "Recognises function application 2" $
        testParse "add True"
          `shouldBe` Right
            ( MyApp
                mempty
                ( MyVar mempty Nothing "add"
                )
                (bool True)
            )

      it "Recognises function application onto namespaced var" $
        testParse "Console.log 1"
          `shouldBe` Right
            ( MyApp
                mempty
                ( MyVar
                    mempty
                    (Just "Console")
                    "log"
                )
                (int 1)
            )

      it "Recognises function application with namespaced arg" $
        testParse "log Prelude.one"
          `shouldBe` Right
            ( MyApp
                mempty
                ( MyVar
                    mempty
                    Nothing
                    "log"
                )
                (MyVar mempty (Just "Prelude") "one")
            )

      it "Recognises function application onto an annotated function" $
        testParse "(\\a -> a: a -> a) True"
          `shouldBe` Right
            ( MyApp
                mempty
                ( MyAnnotation
                    mempty
                    ( MTFunction mempty (MTVar mempty (TVName "a")) (MTVar mempty (TVName "a"))
                    )
                    (MyLambda mempty (Identifier mempty "a") (MyVar mempty Nothing "a"))
                )
                (bool True)
            )
      it "Recognises application with an annotated argument" $
        testParse "id (True: Boolean)"
          `shouldBe` Right
            ( MyApp
                mempty
                (MyVar mempty Nothing "id")
                (MyAnnotation mempty (MTPrim mempty MTBool) (bool True))
            )
      it "Recognises double function application onto a var" $
        testParse "add 1 2"
          `shouldBe` Right
            ( MyApp
                mempty
                ( MyApp
                    mempty
                    ( MyVar mempty Nothing "add"
                    )
                    (int 1)
                )
                (int 2)
            )
      it "Recognises double function application onto a var with brackets" $
        testParse "add (1) (2)"
          `shouldBe` Right
            ( MyApp
                mempty
                ( MyApp
                    mempty
                    ( MyVar mempty Nothing "add"
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
            (MyTuple mempty (int 2) (NE.singleton $ int 2))
      it "Parses a pair of things with silly whitespace" $
        testParse "(     2    ,   2     )"
          `shouldBe` Right
            (MyTuple mempty (int 2) (NE.singleton $ int 2))
      it "Allows a let to use a pair" $
        testParse "let x = ((1,2)) in x"
          `shouldBe` Right
            ( MyLet
                mempty
                (Identifier mempty "x")
                (MyTuple mempty (int 1) (NE.singleton $ int 2))
                (MyVar mempty Nothing "x")
            )
      it "Allows a let to use a pair and apply to it" $
        testParse "let x = ((1,2)) in fst x"
          `shouldBe` Right
            ( MyLet
                mempty
                (Identifier mempty "x")
                (MyTuple mempty (int 1) (NE.singleton $ int 2))
                (MyApp mempty (MyVar mempty Nothing "fst") (MyVar mempty Nothing "x"))
            )
      it "Allows a let to use a nested lambda" $
        testParse "let const2 = (\\a -> (\\b -> a)) in (const2)"
          `shouldBe` Right
            ( MyLet
                mempty
                (Identifier mempty "const2")
                ( MyLambda
                    mempty
                    (Identifier mempty "a")
                    (MyLambda mempty (Identifier mempty "b") (MyVar mempty Nothing "a"))
                )
                (MyVar mempty Nothing "const2")
            )
      it "Parses typed hole" $ do
        testParse "?dog" `shouldBe` Right (MyTypedHole mempty "dog")
      it "Parses a complex let expression" $
        testParse "let const2 = (\\a -> (\\b -> a)) in (let reuse = ({first: const2 True, second: const2 2}) in reuse.second 100)"
          `shouldSatisfy` isRight
      it "Parses an infix equals expression" $
        testParse "True == True" `shouldBe` Right (MyInfix mempty Equals (bool True) (bool True))
      it "Parses two integers with infix operator" $
        testParse "123 == 123" `shouldBe` Right (MyInfix mempty Equals (int 123) (int 123))
      it "Parses var and number equality" $
        testParse " a == 1" `shouldBe` Right (MyInfix mempty Equals (MyVar mempty Nothing "a") (int 1))
      it "Parsers two constructor applications with infix operator" $
        let mkSome = MyApp mempty (MyConstructor mempty Nothing "Some")
         in testParse "(Some 1) == Some 2"
              `shouldBe` Right (MyInfix mempty Equals (mkSome (int 1)) (mkSome (int 2)))
      it "Parses an empty record literal" $
        testParse "{}" `shouldBe` Right (MyRecord mempty mempty)
      it "Parses a record literal with a single item inside" $
        testParse "{ dog: 1 }"
          `shouldBe` Right
            (MyRecord mempty (M.singleton "dog" (int 1)))
      it "Parses a record literal with multiple items inside" $
        testParse "{ dog:1, cat:True, horse:\"of course\" }"
          `shouldBe` Right
            ( MyRecord mempty $
                M.fromList
                  [ ("dog", int 1),
                    ("cat", bool True),
                    ("horse", str' "of course")
                  ]
            )
      it "Parses a record literal with multiple items inside and less spacing" $
        testParse "{dog:1,cat:True,horse:\"of course\"}"
          `shouldBe` Right
            ( MyRecord mempty $
                M.fromList
                  [ ("dog", int 1),
                    ("cat", bool True),
                    ("horse", str' "of course")
                  ]
            )
      it "Parses a record literal with a punned item" $
        testParse "{dog,cat:True}"
          `shouldBe` Right
            ( MyRecord mempty $
                M.fromList
                  [ ("dog", MyVar mempty Nothing "dog"),
                    ("cat", bool True)
                  ]
            )

      it "Parses a destructuring of pairs" $
        testParse "let (a,b) = ((True,1)) in a"
          `shouldBe` Right
            ( MyLetPattern
                mempty
                ( PTuple
                    mempty
                    (PVar mempty "a")
                    (NE.singleton $ PVar mempty "b")
                )
                (MyTuple mempty (bool True) (NE.singleton $ int 1))
                (MyVar mempty Nothing "a")
            )

      it "Parses a multiple argument constructor" $
        testParse "Dog \"hi\" \"dog\""
          `shouldBe` Right
            ( MyApp
                mempty
                ( MyApp
                    mempty
                    (MyConstructor mempty Nothing "Dog")
                    (str' "hi")
                )
                (str' "dog")
            )

      it "Uses a constructor" $
        testParse "Vrai" `shouldBe` Right (MyConstructor mempty Nothing "Vrai")
      it "Uses a namespaced constructor" $
        testParse "LeBool.Vrai" `shouldBe` Right (MyConstructor mempty (Just "LeBool") "Vrai")

      it "Parses big function application" $
        testParse "thing 1 2 3 4 5"
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
                                (MyVar mempty Nothing "thing")
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
      it "Parses big infix fest" $
        testParse "(id 1) + (id 2) + (id 3)" `shouldSatisfy` isRight
      it "Parses infix with annotations" $
        testParse "(1 : Int) + (2 : Int) + (3 : Int)" `shouldSatisfy` isRight
      it "Parses smaller app in If" $
        testParse "if id True then 1 else 2" `shouldSatisfy` isRight
      it "Parses big app in If" $
        testParse "if id True then id 1 else id 2" `shouldSatisfy` isRight
      it "Parses big app with brackets in If" $
        testParse "if (id True) then (id 1) else (id 2)" `shouldSatisfy` isRight
      it "Parser pureState" $
        testParse "\\a -> State (\\s -> Pair a s)"
          `shouldBe` Right
            ( MyLambda
                mempty
                (Identifier mempty "a")
                ( MyApp
                    mempty
                    (MyConstructor mempty Nothing "State")
                    ( MyLambda
                        mempty
                        (Identifier mempty "s")
                        ( MyApp
                            mempty
                            ( MyApp
                                mempty
                                ( MyConstructor mempty Nothing "Pair"
                                )
                                (MyVar mempty Nothing "a")
                            )
                            (MyVar mempty Nothing "s")
                        )
                    )
                )
            )
      it "Nested constructor application" $
        testParse "Log 1 \"dog\" 1"
          `shouldBe` Right
            ( MyApp
                mempty
                ( MyApp
                    mempty
                    ( MyApp
                        mempty
                        (MyConstructor mempty Nothing "Log")
                        (int 1)
                    )
                    (str "dog")
                )
                (int 1)
            )
      it "Nested constructor and function application" $
        testParse "Log 1 (func \"dog\" 1)"
          `shouldBe` Right
            ( MyApp
                mempty
                ( MyApp
                    mempty
                    (MyConstructor mempty Nothing "Log")
                    (int 1)
                )
                ( MyApp
                    mempty
                    ( MyApp
                        mempty
                        (MyVar mempty Nothing "func")
                        (str "dog")
                    )
                    (int 1)
                )
            )

      it "Nested application" $
        testParse "func 1 \"dog\" 1"
          `shouldBe` Right
            ( MyApp
                mempty
                ( MyApp
                    mempty
                    ( MyApp
                        mempty
                        (MyVar mempty Nothing "func")
                        (int 1)
                    )
                    (str "dog")
                )
                (int 1)
            )
      it "Application after brackets" $
        testParse "\\a -> (compose id id) a"
          `shouldBe` Right
            ( MyLambda
                mempty
                (Identifier mempty "a")
                ( MyApp
                    mempty
                    ( MyApp
                        mempty
                        ( MyApp
                            mempty
                            (MyVar mempty Nothing "compose")
                            (MyVar mempty Nothing "id")
                        )
                        (MyVar mempty Nothing "id")
                    )
                    (MyVar mempty Nothing "a")
                )
            )

      it "Tree type value" $
        testParse "Branch (Leaf 1) (Leaf 2)"
          `shouldBe` Right
            ( MyApp
                mempty
                ( MyApp
                    mempty
                    (MyConstructor mempty Nothing "Branch")
                    (MyApp mempty (MyConstructor mempty Nothing "Leaf") (int 1))
                )
                (MyApp mempty (MyConstructor mempty Nothing "Leaf") (int 2))
            )

      it "dog + log" $
        testParse "dog + log"
          `shouldBe` Right (MyInfix mempty Add (MyVar mempty Nothing "dog") (MyVar mempty Nothing "log"))

      it "a+" $
        testParse "a+" `shouldSatisfy` isLeft

      it "a == 1" $
        testParse "a == 1"
          `shouldBe` Right
            (MyInfix mempty Equals (MyVar mempty Nothing "a") (int 1))

      it "a + 1" $
        testParse "a + 1"
          `shouldBe` Right
            (MyInfix mempty Add (MyVar mempty Nothing "a") (int 1))

      it "a - 1" $
        testParse "a - 1"
          `shouldBe` Right
            (MyInfix mempty Subtract (MyVar mempty Nothing "a") (int 1))

      it "a+ 1" $
        testParse "a+ 1"
          `shouldBe` Right
            (MyInfix mempty Add (MyVar mempty Nothing "a") (int 1))

      it "a+1" $
        testParse "a+1"
          `shouldSatisfy` isRight

      it "a  +  b" $
        testParse "a  +  b"
          `shouldBe` Right
            (MyInfix mempty Add (MyVar mempty Nothing "a") (MyVar mempty Nothing "b"))

      it "1 + a" $
        testParse "1 + a"
          `shouldBe` Right
            (MyInfix mempty Add (int 1) (MyVar mempty Nothing "a"))

      it "newName ++ \"!!!\"" $
        testParse "newName ++ \"!!!\""
          `shouldBe` Right (MyInfix mempty StringConcat (MyVar mempty Nothing "newName") (str "!!!"))

      it "Applies a lambda to a function" $
        testParse "map (\\a -> a + 1) [1,2,3]"
          `shouldBe` Right
            ( MyApp
                mempty
                ( MyApp
                    mempty
                    (MyVar mempty Nothing "map")
                    ( MyLambda
                        mempty
                        (Identifier mempty "a")
                        ( MyInfix mempty Add (MyVar mempty Nothing "a") (int 1)
                        )
                    )
                )
                (MyArray mempty [int 1, int 2, int 3])
            )
      it "Parses passing a lambda to a function" $
        testParse "arrayReduce (\\all -> \\a -> [ all ] <> a) []"
          `shouldSatisfy` isRight
      it "Parses big nested thing without boo boos" $
        testParse "let parser = bindParser (\\a -> if a == \"d\" then anyChar else failParser) anyChar; runParser parser \"dog\""
          `shouldBe` Right
            ( MyLet
                mempty
                (Identifier mempty "parser")
                ( MyApp
                    mempty
                    ( MyApp
                        mempty
                        (MyVar mempty Nothing "bindParser")
                        ( MyLambda
                            mempty
                            (Identifier mempty "a")
                            ( MyIf
                                mempty
                                ( MyInfix
                                    mempty
                                    Equals
                                    (MyVar mempty Nothing "a")
                                    (MyLiteral mempty (MyString "d"))
                                )
                                (MyVar mempty Nothing "anyChar")
                                (MyVar mempty Nothing "failParser")
                            )
                        )
                    )
                    (MyVar mempty Nothing "anyChar")
                )
                (MyApp mempty (MyApp mempty (MyVar mempty Nothing "runParser") (MyVar mempty Nothing "parser")) (MyLiteral mempty (MyString "dog")))
            )

      it "parses destructuring a tuple" $
        testParse "let (a,b) = (1,2); a"
          `shouldBe` Right
            ( MyLetPattern
                mempty
                ( PTuple
                    mempty
                    (PVar mempty "a")
                    (NE.singleton $ PVar mempty "b")
                )
                (MyTuple mempty (int 1) (NE.singleton $ int 2))
                (MyVar mempty Nothing "a")
            )
      it "parses destructuring a record with puns" $
        testParse "let {a,b:c} = { a: 1, b: 2}; c"
          `shouldBe` Right
            ( MyLetPattern
                mempty
                ( PRecord
                    mempty
                    ( M.fromList
                        [ ("a", PVar mempty "a"),
                          ("b", PVar mempty "c")
                        ]
                    )
                )
                (MyRecord mempty (M.fromList [("a", int 1), ("b", int 2)]))
                (MyVar mempty Nothing "c")
            )
      it "parses access of a record literal" $ do
        testParse "{ dog: True }.dog"
          `shouldSatisfy` isRight
      it "parses an int with type annotation" $ do
        testParse "(1 : Int)"
          `shouldBe` Right
            ( MyAnnotation
                mempty
                (MTPrim mempty MTInt)
                (MyLiteral mempty (MyInt 1))
            )
      it "parses a function with type annotation" $ do
        testParse "(\\a -> a : Int -> Int)"
          `shouldBe` Right
            ( MyAnnotation
                mempty
                ( MTFunction
                    mempty
                    (MTPrim mempty MTInt)
                    (MTPrim mempty MTInt)
                )
                (MyLambda mempty (Identifier mempty "a") (MyVar mempty Nothing "a"))
            )
      it "parses a let binding with type annotation" $ do
        testParse "let (a: Int) = 1 in True"
          `shouldBe` Right
            ( MyLet
                mempty
                (Identifier mempty "a")
                ( MyAnnotation
                    mempty
                    (MTPrim mempty MTInt)
                    (MyLiteral mempty (MyInt 1))
                )
                (MyLiteral mempty (MyBool True))
            )
      it "parses a let function with type annotation" $ do
        testParse "let (addOne: Int -> Int) a = a + 1 in True"
          `shouldBe` Right
            ( MyLet
                mempty
                (Identifier mempty "addOne")
                ( MyAnnotation
                    mempty
                    ( MTFunction
                        mempty
                        (MTPrim mempty MTInt)
                        (MTPrim mempty MTInt)
                    )
                    ( MyLambda
                        mempty
                        (Identifier mempty "a")
                        ( MyInfix
                            mempty
                            Add
                            (MyVar mempty Nothing "a")
                            (MyLiteral mempty (MyInt 1))
                        )
                    )
                )
                (MyLiteral mempty (MyBool True))
            )

    describe "Test annotations" $ do
      it "Parses a var with location information" $
        testParseWithAnn "dog" `shouldBe` Right (MyVar (Location 0 3) Nothing "dog")
      it "Parses a tyCon with location information" $
        testParseWithAnn "Log" `shouldBe` Right (MyConstructor (Location 0 3) Nothing "Log")
      it "Parses a true bool with location information" $
        testParseWithAnn "True" `shouldBe` Right (MyLiteral (Location 0 4) (MyBool True))
      it "Parses a false bool with location information" $
        testParseWithAnn "False" `shouldBe` Right (MyLiteral (Location 0 5) (MyBool False))
      it "Parses an integer with location information" $
        testParseWithAnn "100" `shouldBe` Right (MyLiteral (Location 0 3) (MyInt 100))
      it "Parses a string literal with location information" $
        testParseWithAnn "\"horse\"" `shouldBe` Right (MyLiteral (Location 0 7) (MyString $ StringType "horse"))
      it "Parses record access with location information" $
        testParseWithAnn "dog.tail"
          `shouldBe` Right
            ( MyRecordAccess
                (Location 0 8)
                (MyVar (Location 0 3) Nothing "dog")
                "tail"
            )
      it "Parses let-in with location information" $
        testParseWithAnn "let a = 1 in a"
          `shouldBe` Right
            ( MyLet
                (Location 0 14)
                (Identifier (Location 4 5) "a")
                (MyLiteral (Location 8 10) (MyInt 1))
                (MyVar (Location 13 14) Nothing "a")
            )
      it "Parses let-newline with location information" $
        testParseWithAnn "let a = 1; a"
          `shouldBe` Right
            ( MyLet
                (Location 0 12)
                (Identifier (Location 4 5) "a")
                (MyLiteral (Location 8 9) (MyInt 1))
                (MyVar (Location 11 12) Nothing "a")
            )
      it "Parsers lambda with location information" $
        testParseWithAnn "\\a -> a"
          `shouldBe` Right
            (MyLambda (Location 0 7) (Identifier (Location 1 2) "a") (MyVar (Location 6 7) Nothing "a"))
      it "Parses application with location information" $
        testParseWithAnn "a 1"
          `shouldBe` Right
            ( MyApp
                (Location 0 3)
                (MyVar (Location 0 2) Nothing "a")
                (MyLiteral (Location 2 3) (MyInt 1))
            )
      it "Parses record with location information" $
        testParseWithAnn "{ a: True }"
          `shouldBe` Right
            ( MyRecord
                (Location 0 11)
                ( M.singleton
                    "a"
                    (MyLiteral (Location 5 10) (MyBool True))
                )
            )
      it "Parsers if with location information" $
        testParseWithAnn "if True then 1 else 2"
          `shouldBe` Right
            ( MyIf
                (Location 0 21)
                (MyLiteral (Location 3 8) (MyBool True))
                (MyLiteral (Location 13 15) (MyInt 1))
                (MyLiteral (Location 20 21) (MyInt 2))
            )
      it "Parsers pair with location information" $
        testParseWithAnn "(1,2)"
          `shouldBe` Right
            ( MyTuple
                (Location 0 5)
                (MyLiteral (Location 1 2) (MyInt 1))
                (NE.singleton $ MyLiteral (Location 3 4) (MyInt 2))
            )
      it "Parses constructor application with location information" $
        testParseWithAnn "Just 1"
          `shouldBe` Right
            ( MyApp
                (Location 0 6)
                (MyConstructor (Location 0 5) Nothing "Just")
                (MyLiteral (Location 5 6) (MyInt 1))
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
        testParseWithAnn "\\a -> if ?tobool a then 1 else 2"
          `shouldSatisfy` isRight
      it "Parser function application in infix" $
        testParseWithAnn "id 1 + 1" `shouldSatisfy` isRight
      it "Accepts whitespace after record" $
        testParseWithAnn "{ name: 1 } " `shouldSatisfy` isRight
      it "Parses array of numbers" $
        testParseWithAnn "[1,2,3]"
          `shouldBe` Right
            ( MyArray
                (Location 0 7)
                [ MyLiteral (Location 1 2) (MyInt 1),
                  MyLiteral (Location 3 4) (MyInt 2),
                  MyLiteral (Location 5 6) (MyInt 3)
                ]
            )
      it "Parses empty array" $
        testParseWithAnn "[]"
          `shouldBe` Right
            ( MyArray (Location 0 2) mempty
            )
    describe "Pattern matching" $ do
      it "Parses wildcard pattern match" $
        testParseWithAnn "match 1 with _ -> True"
          `shouldBe` Right
            ( MyPatternMatch
                (Location 0 22)
                (MyLiteral (Location 6 8) (MyInt 1))
                [ ( PWildcard (Location 13 14),
                    MyLiteral (Location 18 22) (MyBool True)
                  )
                ]
            )
      it "Parses wildcard pattern match with multiple cases" $
        testParseWithAnn "match 1 with _ -> True | _ -> False"
          `shouldBe` Right
            ( MyPatternMatch
                (Location 0 35)
                (MyLiteral (Location 6 8) (MyInt 1))
                [ ( PWildcard (Location 13 14),
                    MyLiteral (Location 18 23) (MyBool True)
                  ),
                  ( PWildcard (Location 25 26),
                    MyLiteral (Location 30 35) (MyBool False)
                  )
                ]
            )
      it "Parses variable pattern match" $
        testParseWithAnn "match 1 with a -> a"
          `shouldBe` Right
            ( MyPatternMatch
                (Location 0 19)
                (MyLiteral (Location 6 8) (MyInt 1))
                [ ( PVar (Location 13 15) "a",
                    MyVar (Location 18 19) Nothing "a"
                  )
                ]
            )
      it "Parses constructor pattern match" $
        testParseWithAnn "match None with None -> False"
          `shouldBe` Right
            ( MyPatternMatch
                (Location 0 29)
                (MyConstructor (Location 6 11) Nothing "None")
                [ ( PConstructor (Location 16 21) Nothing "None" mempty,
                    MyLiteral (Location 24 29) (MyBool False)
                  )
                ]
            )
      it "Parses constructor with arg pattern match" $
        testParseWithAnn "match Some 1 with (Some _) -> True"
          `shouldBe` Right
            ( MyPatternMatch
                (Location 0 34)
                (MyApp (Location 6 13) (MyConstructor (Location 6 11) Nothing "Some") (MyLiteral (Location 11 12) (MyInt 1)))
                [ ( PConstructor (Location 19 25) Nothing "Some" [PWildcard (Location 24 25)],
                    MyLiteral (Location 30 34) (MyBool True)
                  )
                ]
            )
      it "Parses namespaced constructor with arg pattern match" $
        testParseWithAnn "match Maybe.Some 1 with (Maybe.Some _) -> True"
          `shouldBe` Right
            ( MyPatternMatch
                (Location 0 46)
                (MyApp (Location 6 19) (MyConstructor (Location 6 17) (Just "Maybe") "Some") (MyLiteral (Location 17 18) (MyInt 1)))
                [ ( PConstructor (Location 25 37) (Just "Maybe") "Some" [PWildcard (Location 36 37)],
                    MyLiteral (Location 42 46) (MyBool True)
                  )
                ]
            )

    describe "Parse regressions" $ do
      it "regression 1" $
        testParse "let a = 1; let b = a + 1 in match True with True -> 1 | False -> 2"
          `shouldSatisfy` isRight
      it "regression 2" $
        testParse "let stringReduce = \\f -> \\defVal -> \\str -> match str with \"\" -> defVal | head ++ tail -> stringReduce f (f defVal head) tail; stringReduce"
          `shouldSatisfy` isRight
