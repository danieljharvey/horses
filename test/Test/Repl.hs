{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Repl
  ( spec,
  )
where

import Data.Either (isLeft)
import Data.Text (Text)
import Language.Mimsa.Interpreter
import Language.Mimsa.Repl
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec
import Test.StoreData

eval :: Project -> Text -> IO (Either Text (MonoType, Expr Variable))
eval env input =
  case evaluateText env input of
    Right (mt, expr', scope') -> do
      endExpr <- interpret scope' expr'
      case endExpr of
        Right a -> pure (Right (mt, a))
        Left e -> pure (Left (prettyPrint $ InterpreterErr e))
    Left e -> pure (Left $ prettyPrint e)

spec :: Spec
spec =
  describe "Repl"
    $ describe "End to end parsing to evaluation"
    $ do
      it "let x = ((1,2)) in fst(x)" $ do
        result <- eval stdLib "let x = ((1,2)) in fst(x)"
        result
          `shouldBe` Right
            (MTInt, int 1)
      it "let good = { dog: True } in good.dog" $ do
        result <- eval stdLib "let good = ({ dog: True }) in good.dog"
        result `shouldBe` Right (MTBool, bool True)
      it "let prelude = { id: (\\i -> i) } in prelude.id" $ do
        result <- eval stdLib "let prelude = ({ id: (\\i -> i) }) in prelude.id"
        result
          `shouldBe` Right
            ( MTFunction (unknown 3) (unknown 3),
              MyLambda (named "i") (MyVar (named "i"))
            )
      it "let prelude = ({ id: (\\i -> i) }) in prelude.id(1)" $ do
        result <- eval stdLib "let prelude = ({ id: (\\i -> i) }) in prelude.id(1)"
        result
          `shouldBe` Right
            ( MTInt,
              int 1
            )
      it "let bigPrelude = ({ prelude: { id: (\\i -> i) } }) in bigPrelude.prelude.id(1)" $ do
        result <- eval stdLib "let bigPrelude = ({ prelude: { id: (\\i -> i) } }) in bigPrelude.prelude.id(1)"
        result
          `shouldBe` Right
            ( MTInt,
              int 1
            )
      it "let compose = (\\f -> \\g -> \\a -> f(g(a))) in compose(incrementInt)(incrementInt)(67)" $ do
        result <- eval mempty "let compose = (\\f -> \\g -> \\a -> f(g(a))) in compose(incrementInt)(incrementInt)(67)"
        result `shouldBe` Right (MTInt, int 69)
      it "let reuse = ({ first: id(1), second: id(2) }) in reuse.first" $ do
        result <- eval stdLib "let reuse = ({ first: id(1), second: id(2) }) in reuse.first"
        result `shouldBe` Right (MTInt, int 1)
      it "let id = \\a -> a in id(1)" $ do
        result <- eval mempty "let id = \\a -> a in id(1)"
        result `shouldBe` Right (MTInt, int 1)
      it "let reuse = ({ first: id(True), second: id(2) }) in reuse.first" $ do
        result <- eval stdLib "let reuse = ({ first: id(True), second: id(2) }) in reuse.first"
        result `shouldBe` Right (MTBool, bool True)
      it "let reuse = ({ first: id, second: id(2) }) in reuse.first(True)" $ do
        result <- eval stdLib "let reuse = ({ first: id, second: id(2) }) in reuse.first(True)"
        result `shouldBe` Right (MTBool, bool True)
      it "let const2 = \\a -> \\b -> a in (let reuse = ({ first: const2(1), second: const2(True) }) in reuse.first(100))" $ do
        result <- eval stdLib "let const2 = \\a -> \\b -> a in (let reuse = ({ first: const2(1), second: const2(True) }) in reuse.first(100))"
        result `shouldBe` Right (MTInt, int 1)
      it "let const2 = \\a -> \\b -> a in (let reuse = ({ first: const2(True), second: const2(2) }) in reuse.second(100))" $ do
        result <- eval stdLib "let const2 = \\a -> \\b -> a in (let reuse = ({ first: const2(True), second: const2(2) }) in reuse.second(100))"
        result `shouldBe` Right (MTInt, int 2)
      it "addInt(1)(2)" $ do
        result <- eval stdLib "addInt(1)(2)"
        result `shouldBe` Right (MTInt, int 3)
      it "(\\a -> a)(1)" $ do
        result <- eval stdLib "(\\a -> a)(1)"
        result `shouldBe` Right (MTInt, int 1)
      it "(\\b -> (\\a -> b))(0)(1)" $ do
        result <- eval stdLib "(\\b -> (\\a -> b))(0)(1)"
        result `shouldBe` Right (MTInt, int 0)
      it "addInt(1)(addInt(addInt(2)(4))(5))" $ do
        result <- eval stdLib "addInt(1)(addInt(addInt(2)(4))(5))"
        result `shouldBe` Right (MTInt, int 12)
      it "type LeBool = Vrai | Faux in Vrai" $ do
        result <- eval stdLib "type LeBool = Vrai | Faux in Vrai"
        result
          `shouldBe` Right
            ( MTData (mkConstruct "LeBool") [],
              MyConstructor (mkConstruct "Vrai")
            )
      it "type Nat = Zero | Suc Nat in Suc Zero" $ do
        result <- eval stdLib "type Nat = Zero | Suc Nat in Suc Zero"
        result
          `shouldBe` Right
            ( MTData (mkConstruct "Nat") [],
              MyConsApp (MyConstructor (mkConstruct "Suc")) (MyConstructor (mkConstruct "Zero"))
            )
      it "type Nat = Zero | Suc Nat in Suc Suc Zero" $ do
        result <- eval stdLib "type Nat = Zero | Suc Nat in Suc Suc Zero"
        result
          `shouldBe` Right
            ( MTData (mkConstruct "Nat") [],
              MyConsApp
                (MyConstructor (mkConstruct "Suc"))
                ( MyConsApp (MyConstructor (mkConstruct "Suc")) (MyConstructor (mkConstruct "Zero"))
                )
            )
      it "type Nat = Zero | Suc Nat in Suc 1" $ do
        result <- eval stdLib "type Nat = Zero | Suc Nat in Suc 1"
        result
          `shouldSatisfy` isLeft
      it "type Nat = Zero | Suc Nat in Suc Dog" $ do
        result <- eval stdLib "type Nat = Zero | Suc Nat in Suc Dog"
        result
          `shouldSatisfy` isLeft
      it "type Nat = Zero | Suc Nat in Suc" $ do
        result <- eval stdLib "type Nat = Zero | Suc Nat in Suc"
        result
          `shouldBe` Right
            ( MTFunction
                (MTData (mkConstruct "Nat") [])
                (MTData (mkConstruct "Nat") []),
              MyConstructor (mkConstruct "Suc")
            )
      it "type OhNat = Zero | Suc OhNat String in Suc" $ do
        result <- eval stdLib "type OhNat = Zero | Suc OhNat String in Suc"
        result
          `shouldBe` Right
            ( MTFunction
                (MTData (mkConstruct "OhNat") [])
                ( MTFunction
                    MTString
                    (MTData (mkConstruct "OhNat") [])
                ),
              MyConstructor (mkConstruct "Suc")
            )
      it "type Pet = Cat String | Dog String in Cat \"mimsa\"" $ do
        result <- eval stdLib "type Pet = Cat String | Dog String in Cat \"mimsa\""
        result
          `shouldBe` Right
            ( MTData (mkConstruct "Pet") [],
              MyConsApp (MyConstructor (mkConstruct "Cat")) (str' "mimsa")
            )
      it "type Void in 1" $ do
        result <- eval stdLib "type Void in 1"
        result `shouldBe` Right (MTInt, int 1)
      it "type String = Should | Error in Error" $ do
        result <- eval stdLib "type String = Should | Error in Error"
        result `shouldSatisfy` isLeft
      it "type LongBoy = Stuff String Int String in Stuff \"yes\"" $ do
        result <- eval stdLib "type LongBoy = Stuff String Int String in Stuff \"yes\""
        result
          `shouldBe` Right
            ( MTFunction
                MTInt
                ( MTFunction
                    MTString
                    (MTData (mkConstruct "LongBoy") [])
                ),
              MyConsApp
                (MyConstructor (mkConstruct "Stuff"))
                (str' "yes")
            )
      it "type Tree = Leaf Int | Branch Tree Tree in Branch (Leaf 1) (Leaf 2)" $ do
        result <- eval stdLib "type Tree = Leaf Int | Branch Tree Tree in Branch (Leaf 1) (Leaf 2)"
        result
          `shouldBe` Right
            ( MTData (mkConstruct "Tree") [],
              MyConsApp
                ( MyConsApp
                    (MyConstructor $ mkConstruct "Branch")
                    (MyConsApp (MyConstructor $ mkConstruct "Leaf") (int 1))
                )
                (MyConsApp (MyConstructor $ mkConstruct "Leaf") (int 2))
            )
      it "type Maybe a = Just a | Nothing in Just" $ do
        result <- eval stdLib "type Maybe a = Just a | Nothing in Just"
        result
          `shouldBe` Right
            ( MTFunction
                (MTVar (NumberedVar 1))
                (MTData (mkConstruct "Maybe") [MTVar (NumberedVar 1)]),
              MyConstructor $ mkConstruct "Just"
            )
      it "type Maybe a = Just a | Nothing in Nothing" $ do
        result <- eval stdLib "type Maybe a = Just a | Nothing in Nothing"
        result
          `shouldBe` Right
            ( MTData (mkConstruct "Maybe") [MTVar (NumberedVar 1)],
              MyConstructor $ mkConstruct "Nothing"
            )
      it "type Maybe a = Just a | Nothing in Just 1" $ do
        result <- eval stdLib "type Maybe a = Just a | Nothing in Just 1"
        result
          `shouldBe` Right
            ( MTData (mkConstruct "Maybe") [MTInt],
              MyConsApp (MyConstructor $ mkConstruct "Just") (int 1)
            )
      it "type Maybe a = Just a | Nothing in case Just 1 of Just \\a -> eq(100)(a) | Nothing False" $ do
        result <- eval stdLib "type Maybe a = Just a | Nothing in case Just 1 of Just \\a -> eq(100)(a) | Nothing False"
        result
          `shouldBe` Right
            (MTBool, bool False)
      it "type Maybe a = Just a | Nothing in case Just 1 of Just \\a -> True | Nothing 1" $ do
        result <- eval stdLib "type Maybe a = Just a | Nothing in case Just 1 of Just \\a -> True | Nothing 1"
        result `shouldSatisfy` isLeft
      it "type Maybe a = Just a | Nothing in case Just 1 of Just \\a -> eq(100)(a) | otherwise False" $ do
        result <- eval stdLib "type Maybe a = Just a | Nothing in case Just 1 of Just \\a -> eq(100)(a) | otherwise False"
        result
          `shouldBe` Right
            (MTBool, bool False)
      it "type Stuff = Thing String Int in case Thing \"Hello\" 1 of Thing \\name -> \\num -> name" $ do
        result <- eval stdLib "type Stuff = Thing String Int in case Thing \"Hello\" 1 of Thing \\name -> \\num -> name"
        result
          `shouldBe` Right
            (MTString, str' "Hello")
      it "type Result e a = Failure e | Success a in case Failure \"oh no\" of Success \\a -> \"oh yes\" | Failure \\e -> e" $ do
        result <- eval stdLib "type Result e a = Failure e | Success a in case Failure \"oh no\" of Success \\a -> \"oh yes\" | Failure \\e -> e"
        result
          `shouldBe` Right
            (MTString, str' "oh no")
      it "type Blap a = Boop a Int in case Boop True 100 of Boop \\a -> \\b -> a" $ do
        result <- eval stdLib "type Blap a = Boop a Int in case Boop True 100 of Boop \\a -> \\b -> a"
        result `shouldBe` Right (MTBool, bool True)
      it "type Maybe a = Just a | Nothing in case Nothing of Nothing False" $ do
        result <- eval stdLib "type Maybe a = Just a | Nothing in case Nothing of Nothing False"
        result `shouldSatisfy` isLeft
      it "type Thing = Thing String in let a = Thing \"string\" in case a of Thing \\s -> s" $ do
        result <- eval stdLib "type Thing = Thing String in let a = Thing \"string\" in case a of Thing \\s -> s"
        result `shouldBe` Right (MTString, str' "string")
      it "type Pair a b = Pair a b in case Pair \"dog\" 1 of Pair \a -> a" $ do
        result <- eval stdLib "type Pair a b = Pair a b in case Pair \"dog\" 1 of Pair \a -> a"
        result `shouldSatisfy` isLeft
      it "type Tree a = Leaf a | Branch (Tree a) (Tree a) in Leaf 1" $ do
        result <- eval stdLib "type Tree a = Leaf a | Branch (Tree a) (Tree a) in Leaf 1"
        result
          `shouldBe` Right
            ( MTData (mkConstruct "Tree") [MTInt],
              MyConsApp (MyConstructor $ mkConstruct "Leaf") (int 1)
            )
      {-
      it "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Leaf 1" $ do
        result <- eval stdLib "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Leaf 1"
        result
          `shouldSatisfy` isLeft
      -}
      it "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Branch (Leaf 1) (Leaf True)" $ do
        result <- eval stdLib "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Branch (Leaf 1) (Leaf True)"
        result
          `shouldSatisfy` isLeft
      it "type Tree a = Empty | Branch (Tree a) a (Tree a) in Branch (Empty) 1 (Empty)" $ do
        result <- eval stdLib "type Tree a = Empty | Branch (Tree a) a (Tree a) in Branch (Empty) 1 (Empty)"
        result
          `shouldBe` Right
            ( MTData (mkConstruct "Tree") [MTInt],
              MyConsApp
                ( MyConsApp
                    ( MyConsApp
                        (MyConstructor $ mkConstruct "Branch")
                        (MyConstructor $ mkConstruct "Empty")
                    )
                    (int 1)
                )
                (MyConstructor $ mkConstruct "Empty")
            )
      it "type Maybe a = Just a | Nothing in case Just True of Just \\a -> a | Nothing \"what\"" $ do
        result <- eval stdLib "type Maybe a = Just a | Nothing in case Just True of Just \\a -> a | Nothing \"what\""
        result `shouldSatisfy` isLeft
