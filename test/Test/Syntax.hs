{-# LANGUAGE OverloadedStrings #-}

module Test.Syntax
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Language.Mimsa.Syntax
import qualified Language.Mimsa.Syntax as P
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser" $ do
    it "thenSpace match" $
      P.runParser (P.thenSpace (P.literal "let")) "let " `shouldBe` Right ("", "let")
    it "thenSpace mismatch" $
      isLeft (P.runParser (P.thenSpace (P.literal "let")) "let") `shouldBe` True
    it "applicative" $ do
      let parser = (,) <$> P.literal "one" <*> P.literal "two"
      P.runParser parser "onetwo" `shouldBe` Right ("", ("one", "two"))
    it "applicative with thenSpace" $ do
      let parser = (,) <$> P.thenSpace (P.literal "one") <*> P.thenSpace (P.literal "two")
      P.runParser parser "one      two " `shouldBe` Right ("", ("one", "two"))
  describe "Language" $ do
    it "Parses True" $
      parseExpr "True" `shouldBe` Right (bool True)
    it "Parses False" $
      parseExpr "False" `shouldBe` Right (bool False)
    it "Parses 6" $
      parseExpr "6" `shouldBe` Right (int 6)
    it "Parses 1234567" $
      parseExpr "1234567" `shouldBe` Right (int 1234567)
    it "Parses -6" $
      parseExpr "-6" `shouldBe` Right (int (-6))
    it "Parses +6" $
      parseExpr "+6" `shouldBe` Right (int 6)
    it "Parses a string" $
      parseExpr "\"dog\"" `shouldBe` Right (str (StringType "dog"))
    it "Parses a variable name" $
      parseExpr "log"
        `shouldBe` Right (MyVar (mkName "log"))
    it "Does not accept 'let' as a variable name" $
      isLeft (parseExpr "let")
        `shouldBe` True
    it "Does not accept 'in' as a variable name" $
      isLeft (parseExpr "in")
        `shouldBe` True
    it "Does not accept 2log as a variable name because it starts with a number" $
      isLeft (parseExpr "2log") `shouldBe` True
    it "Does not recognise a stupid variable name with crap in it" $
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
    it "Recognises a basic lambda" $
      parseExpr "\\x -> x"
        `shouldBe` Right (MyLambda (mkName "x") (MyVar (mkName "x")))
    it "Recognises a lambda with too much whitespace everywhere" $
      parseExpr "\\        x          ->             x"
        `shouldBe` Right (MyLambda (mkName "x") (MyVar (mkName "x")))
    it "Recognises a lambda in parens" $
      parseExpr "(\\x -> x)"
        `shouldBe` Right (MyLambda (mkName "x") (MyVar (mkName "x")))
    it "Recognises nested lambdas in parens" $
      parseExpr "(\\a -> (\\b -> a))"
        `shouldBe` Right
          ( MyLambda
              (mkName "a")
              (MyLambda (mkName "b") (MyVar (mkName "a")))
          )
    it "Recognises function application in parens" $
      parseExpr "add (1)"
        `shouldBe` Right
          ( MyApp
              ( MyVar (mkName "add")
              )
              (int 1)
          )
    it "Recognises double function application onto a var" $
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
    it "Parses a pair of things" $
      parseExpr "(2, 2)"
        `shouldBe` Right
          (MyPair (int 2) (int 2))
    it "Parses a pair of things with silly whitespace" $
      parseExpr "(     2    ,   2     )"
        `shouldBe` Right
          (MyPair (int 2) (int 2))
    it "Allows a let to use a pair" $
      parseExpr "let x = ((1,2)) in x"
        `shouldBe` Right
          ( MyLet
              (mkName "x")
              (MyPair (int 1) (int 2))
              (MyVar (mkName "x"))
          )
    it "Allows a let to use a pair and apply to it" $
      parseExpr "let x = ((1,2)) in fst(x)"
        `shouldBe` Right
          ( MyLet
              (mkName "x")
              (MyPair (int 1) (int 2))
              (MyApp (MyVar (mkName "fst")) (MyVar (mkName "x")))
          )
    it "Allows a let to use a nested lambda" $
      parseExpr "let const2 = (\\a -> (\\b -> a)) in (const2)"
        `shouldBe` Right
          ( MyLet
              (mkName "const2")
              ( MyLambda
                  (mkName "a")
                  (MyLambda (mkName "b") (MyVar (mkName "a")))
              )
              (MyVar (mkName "const2"))
          )
    it "Parses a complex let expression" $
      parseExpr "let const2 = (\\a -> (\\b -> a)) in (let reuse = ({first: const2(True), second: const2(2)}) in reuse.second(100))"
        `shouldSatisfy` isRight
    it "Parses an empty record literal" $
      parseExpr "{}" `shouldBe` Right (MyRecord mempty)
    it "Parses a record literal with a single item inside" $
      parseExpr "{ dog: 1 }"
        `shouldBe` Right
          (MyRecord (M.singleton (mkName "dog") (int 1)))
    it "Parses a record literal with multiple items inside" $
      parseExpr "{ dog:1, cat:True, horse:\"of course\" }"
        `shouldBe` Right
          ( MyRecord $
              M.fromList
                [ (mkName "dog", int 1),
                  (mkName "cat", bool True),
                  (mkName "horse", str' "of course")
                ]
          )
    it "Parses a record literal with multiple items inside and less spacing" $
      parseExpr "{dog:1,cat:True,horse:\"of course\"}"
        `shouldBe` Right
          ( MyRecord $
              M.fromList
                [ (mkName "dog", int 1),
                  (mkName "cat", bool True),
                  (mkName "horse", str' "of course")
                ]
          )
    it "Parses a destructuring of pairs" $
      parseExpr' "let (a,b) = ((True,1)) in a"
        `shouldBe` Right
          ( MyLetPair
              (mkName "a")
              (mkName "b")
              (MyPair (bool True) (int 1))
              (MyVar (mkName "a"))
          )
    it "Parses a destructuring of pairs with silly whitespace" $
      parseExpr' "let   (    a ,      b ) =    ((       True, 1) ) in a"
        `shouldBe` Right
          ( MyLetPair
              (mkName "a")
              (mkName "b")
              (MyPair (bool True) (int 1))
              (MyVar (mkName "a"))
          )
    it "Parses an absolute unit" $
      parseExpr "type AbsoluteUnit = AbsoluteUnit in 1"
        `shouldBe` Right
          ( MyData
              ( DataType
                  (mkConstruct "AbsoluteUnit")
                  mempty
                  (M.singleton (mkConstruct "AbsoluteUnit") mempty)
              )
              (int 1)
          )
    it "Parses a single constructor with one arg" $
      parseExpr "type Dog = Dog String in 1"
        `shouldBe` Right
          ( MyData
              ( DataType
                  (mkConstruct "Dog")
                  mempty
                  ( M.singleton
                      (mkConstruct "Dog")
                      [ConsName (mkConstruct "String") mempty]
                  )
              )
              (int 1)
          )
    it "Parses a french boolean" $
      parseExpr "type LeBool = Vrai | Faux in 1"
        `shouldBe` Right
          ( MyData
              ( DataType
                  (mkConstruct "LeBool")
                  mempty
                  ( M.fromList
                      [ (mkConstruct "Vrai", []),
                        (mkConstruct "Faux", [])
                      ]
                  )
              )
              (int 1)
          )
    it "Parses a peano number data declaration" $
      parseExpr "type Nat = Zero | Succ Nat in 1"
        `shouldBe` Right
          ( MyData
              ( DataType
                  (mkConstruct "Nat")
                  mempty
                  ( M.fromList
                      [ (mkConstruct "Zero", []),
                        (mkConstruct "Succ", [ConsName (mkConstruct "Nat") mempty])
                      ]
                  )
              )
              (int 1)
          )
    it "Parses a multiple argument constructor" $
      parseExpr "Dog \"hi\" \"dog\""
        `shouldBe` Right
          ( MyConsApp
              ( MyConsApp
                  (MyConstructor $ mkConstruct "Dog")
                  (str' "hi")
              )
              (str' "dog")
          )
    it "Parses a type declaration with variable" $
      parseExpr "type Maybe a = Just a | Nothing in Nothing"
        `shouldBe` Right
          ( MyData
              ( DataType
                  (mkConstruct "Maybe")
                  [mkName "a"]
                  ( M.fromList
                      [ (mkConstruct "Just", [VarName $ mkName "a"]),
                        (mkConstruct "Nothing", [])
                      ]
                  )
              )
              (MyConstructor $ mkConstruct "Nothing")
          )
    it "Uses a constructor" $
      parseExpr "Vrai" `shouldBe` Right (MyConstructor (mkConstruct "Vrai"))
    it "Parses a custom case match" $
      parseExpr "case Just 1 of Just \\a -> a | Nothing 0"
        `shouldBe` Right
          ( MyCaseMatch
              (MyConsApp (MyConstructor $ mkConstruct "Just") (int 1))
              ( NE.fromList
                  [ (mkConstruct "Just", MyLambda (mkName "a") (MyVar (mkName "a"))),
                    (mkConstruct "Nothing", int 0)
                  ]
              )
              Nothing
          )
    it "Parses a custom case match with fall through case" $
      parseExpr "case Just 1 of Just \\a -> a | otherwise 0"
        `shouldBe` Right
          ( MyCaseMatch
              (MyConsApp (MyConstructor $ mkConstruct "Just") (int 1))
              ( NE.fromList
                  [ (mkConstruct "Just", MyLambda (mkName "a") (MyVar (mkName "a")))
                  ]
              )
              (Just $ int 0)
          )
    it "Parses complex type constructors" $
      parseExpr "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Leaf 1"
        `shouldBe` Right
          ( MyData
              ( DataType
                  (mkConstruct "Tree")
                  [mkName "a"]
                  ( M.fromList
                      [ (mkConstruct "Leaf", [VarName $ mkName "a"]),
                        ( mkConstruct "Branch",
                          [ ConsName (mkConstruct "Tree") [VarName $ mkName "a"],
                            ConsName (mkConstruct "Tree") [VarName $ mkName "b"]
                          ]
                        )
                      ]
                  )
              )
              (MyConsApp (MyConstructor $ mkConstruct "Leaf") (int 1))
          )
    it "Parses even more complex type constructors" $
      parseExpr "type Tree a = Empty | Branch (Tree a) a (Tree a) in Branch (Empty) 1 (Empty)"
        `shouldBe` Right
          ( MyData
              ( DataType
                  (mkConstruct "Tree")
                  [mkName "a"]
                  ( M.fromList
                      [ (mkConstruct "Empty", mempty),
                        ( mkConstruct "Branch",
                          [ ConsName (mkConstruct "Tree") [VarName $ mkName "a"],
                            VarName $ mkName "a",
                            ConsName (mkConstruct "Tree") [VarName $ mkName "a"]
                          ]
                        )
                      ]
                  )
              )
              ( MyConsApp
                  ( MyConsApp
                      ( MyConsApp
                          (MyConstructor $ mkConstruct "Branch")
                          (MyConstructor $ mkConstruct "Empty")
                      )
                      (int 1)
                  )
                  (MyConstructor $ mkConstruct "Empty")
              )
          )
    it "Parses big function application" $
      parseExpr "thing(1)(2)(3)(4)(5)"
        `shouldBe` Right
          ( MyApp
              ( MyApp
                  ( MyApp
                      ( MyApp
                          ( MyApp
                              (MyVar (mkName "thing"))
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
