{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Repl
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import qualified Data.List.NonEmpty as NE
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
      it "\\x -> if x then Left 1 else Right \"yes\"" $ do
        result <- eval stdLib "\\x -> if x then Right \"yes\" else Left 1"
        result
          `shouldBe` Right
            ( MTFunction MTBool (MTSum MTInt MTString),
              MyLambda
                (named "x")
                ( MyIf
                    (MyVar (named "x"))
                    (MySum MyRight (str' "yes"))
                    (MySum MyLeft (int 1))
                )
            )
      it "case isTen(9) of Left (\\l -> \"It's not ten\") | Right (\\r -> \"It's ten!\")" $ do
        result <- eval stdLib "case isTen(9) of Left (\\l -> \"It's not ten\") | Right (\\r -> \"It's ten!\")"
        result
          `shouldBe` Right
            (MTString, str' "It's not ten")
        result2 <- eval stdLib "case isTen(10) of Left (\\l -> \"It's not ten\") | Right (\\r -> \"It's ten!\")"
        result2
          `shouldBe` Right
            (MTString, str' "It's ten!")
      it "case (Left 1) of Left (\\l -> Left l) | Right (\\r -> Right r)" $ do
        result <- eval stdLib "case (Left 1) of Left (\\l -> Left l) | Right (\\r -> Right r)"
        result
          `shouldBe` Right
            ( MTSum MTInt (unknown 1),
              MySum MyLeft (int 1)
            )
      it "\\sum -> case sum of Left (\\l -> Left l) | Right (\\r -> Right eqTen(r))" $ do
        result <- eval stdLib "\\sum -> case sum of Left (\\l -> Left l) | Right (\\r -> Right eqTen(r))"
        result
          `shouldBe` Right
            ( MTFunction
                (MTSum (unknown 11) MTInt)
                (MTSum (unknown 11) MTBool),
              MyLambda
                (named "sum")
                ( MyCase
                    (MyVar (named "sum"))
                    ( MyLambda
                        (named "l")
                        (MySum MyLeft (MyVar (named "l")))
                    )
                    ( MyLambda
                        (named "r")
                        ( MySum
                            MyRight
                            ( MyApp
                                (MyVar (NumberedVar 0))
                                (MyVar (named "r"))
                            )
                        )
                    )
                )
            )
      it "let sum = (Left 1) in ((fmapSum eqTen) sum)" $ do
        result <- eval stdLib "let sum = (Left 1) in fmapSum (eqTen) (sum)"
        result
          `shouldBe` Right
            (MTSum MTInt MTBool, MySum MyLeft (int 1))
      it "let [head, tail] = ([1,2,3]) in head" $ do
        result <- eval stdLib "let [head, tail] = ([1,2,3]) in head"
        result
          `shouldBe` Right (MTInt, int 1)
      it "let [head, tail] = ([1,2,3]) in tail" $ do
        result <- eval stdLib "let [head, tail] = ([1,2,3]) in tail"
        result
          `shouldBe` Right
            ( MTSum MTUnit (MTList MTInt),
              MySum MyRight $ MyList $ NE.fromList [int 2, int 3]
            )
      it "listUncons([1,2,3])" $ do
        result <- eval stdLib "listUncons([1,2,3])"
        result
          `shouldBe` Right
            ( MTPair MTInt (MTSum MTUnit (MTList MTInt)),
              MyPair
                (int 1)
                ( MySum
                    MyRight
                    ( MyList $
                        NE.fromList [int 2, int 3]
                    )
                )
            )
      it "let listHead = (compose(fst)(listUncons)) in listHead([1,2,3])" $ do
        result <- eval stdLib "let listHead = (compose(fst)(listUncons)) in listHead([1,2,3])"
        result `shouldBe` Right (MTInt, int 1)
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
      it "listHead([1])" $
        do
          result <- eval stdLib "listHead([1])"
          result `shouldBe` Right (MTInt, int 1)
      it "list.head([1])" $ do
        result <- eval stdLib "list.head([1])"
        result `shouldBe` Right (MTInt, int 1)
      it "(listHead,listTail)" $ do
        result <- eval stdLib "(listHead,listTail)"
        result `shouldSatisfy` isRight
      it "listTail([1])" $ do
        result <- eval stdLib "listTail([1])"
        result `shouldBe` Right (MTSum MTUnit (MTList MTInt), MySum MyLeft (MyLiteral MyUnit))
      it "list.tail([1])" $ do
        result <- eval stdLib "list.tail([1])"
        result2 <- eval stdLib "listTail([1])"
        result `shouldBe` result2
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
      it "let x = (maybe.nothing) in maybe.just(1)" $ do
        result <- eval stdLib "let x = (maybe.nothing) in maybe.just(1)"
        result
          `shouldBe` Right
            ( MTSum (MTVar (NumberedVar 14)) MTInt,
              MySum MyRight (int 1)
            )
      it "appendList([1])([2])" $ do
        result <- eval stdLib "appendList([1])([2])"
        result `shouldBe` Right (MTList MTInt, MyList (NE.fromList [int 1, int 2]))
      it "addInt(1)(2)" $ do
        result <- eval stdLib "addInt(1)(2)"
        result `shouldBe` Right (MTInt, int 3)
      it "(\\a -> a)(1)" $ do
        result <- eval stdLib "(\\a -> a)(1)"
        result `shouldBe` Right (MTInt, int 1)
      it "(\\b -> (\\a -> b))(0)(1)" $ do
        result <- eval stdLib "(\\b -> (\\a -> b))(0)(1)"
        result `shouldBe` Right (MTInt, int 0)
      it "let combine = \\b -> \\a -> \"horse\" in reduceList(combine)(\"\")([1])" $ do
        result <- eval stdLib "let combine = \\b -> \\a -> \"horse\" in reduceList(combine)(\"\")([1])"
        result `shouldBe` Right (MTString, str' "horse")
      it "let combine = \\b -> \\a -> \"horse\" in reduceList(combine)(\"\")([1,2,3])" $ do
        result <- eval stdLib "let combine = \\b -> \\a -> \"horse\" in reduceList(combine)(\"\")([1,2,3])"
        result `shouldBe` Right (MTString, str' "horse")
      it "reduceList(\\aa -> \\bb -> addInt(aa)(bb))(0)([1,2,3,4,5])" $ do
        result <- eval stdLib "reduceList(\\aa -> \\bb -> addInt(aa)(bb))(0)([1,2,3,4,5])"
        result `shouldBe` Right (MTInt, int 15)
      it "addInt(1)(addInt(addInt(2)(4))(5))" $ do
        result <- eval stdLib "addInt(1)(addInt(addInt(2)(4))(5))"
        result `shouldBe` Right (MTInt, int 12)
      it "mapList(incrementInt)([1,2,3])" $ do
        result <- eval stdLib "mapList(incrementInt)([1,2,3])"
        result `shouldBe` Right (MTList MTInt, MyList (NE.fromList [int 2, int 3, int 4]))
      it "foldList(addInt)([1,2,3])" $ do
        result <- eval stdLib "foldList(addInt)([1,2,3])"
        result `shouldBe` Right (MTInt, int 6)
      it "listFilter(\\a -> eq(10)(a))([1])" $ do
        result <- eval stdLib "listFilter(\\a -> eq(10)(a))([1])"
        result `shouldBe` Right (MTSum MTUnit (MTList MTInt), MySum MyLeft (MyLiteral MyUnit))
      it "listFilter(\\a -> eq(10)(a))([10,10,30])" $ do
        result <- eval stdLib "listFilter(\\a -> eq(10)(a))([10,10,30])"
        result
          `shouldBe` Right
            ( MTSum MTUnit (MTList MTInt),
              MySum MyRight (MyList (NE.fromList [int 10, int 10]))
            )
      it "foldList(appendList)([[1,2],[3,4]])" $ do
        result <- eval stdLib "foldList(appendList)([[1,2],[3,4]])"
        result
          `shouldBe` Right
            (MTList MTInt, MyList $ NE.fromList [int 1, int 2, int 3, int 4])
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
      it "type Maybe a = Just a | Nothing in Just 1" $ do
        result <- eval stdLib "type Maybe a = Just a | Nothing in Just 1"
        result
          `shouldBe` Right
            ( MTData (mkConstruct "Maybe") [MTInt],
              MyConsApp (MyConstructor $ mkConstruct "Just") (int 1)
            )
