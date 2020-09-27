{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Repl
  ( spec,
  )
where

import Data.Coerce
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Interpreter
import Language.Mimsa.Parser (parseExpr)
import Language.Mimsa.Printer
import Language.Mimsa.Repl
import Language.Mimsa.Types
import Test.Data.Project
import Test.Helpers
import Test.Hspec

eval :: Project -> Text -> IO (Either Text (MonoType, Expr Variable))
eval env input =
  case prettyPrintingParses input of
    Left e -> pure (Left $ prettyPrint e)
    Right _ ->
      case evaluateText env input of
        Left e -> pure (Left $ prettyPrint e)
        Right (ResolvedExpression mt se expr' scope' swaps) -> do
          T.putStrLn $ coerce (output (storeExpression se))
          endExpr <- interpret scope' swaps expr'
          case endExpr of
            Right a -> pure (Right (mt, a))
            Left e -> pure (Left (prettyPrint $ InterpreterErr e))

-- does the output of our prettyprinting still make sense to the parser?
prettyPrintingParses :: Text -> Either Text ()
prettyPrintingParses input = do
  expr1 <- parseExpr input
  case parseExpr (prettyPrint expr1) of
    Left _ -> Left ("Could not parse >>>" <> prettyPrint expr1 <> "<<<")
    Right expr2 ->
      if expr1 /= expr2
        then
          Left
            ( ">>>" <> T.pack (show expr1)
                <> "<<< does not match >>>"
                <> T.pack (show expr2)
                <> "<<< "
                <> prettyPrint expr1
                <> " vs "
                <> prettyPrint expr2
            )
        else pure ()

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
            ( MTFunction (unknown 4) (unknown 4),
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
        result <- eval stdLib "let compose = (\\f -> \\g -> \\a -> f(g(a))) in compose(incrementInt)(incrementInt)(67)"
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
            ( MTData (mkTyCon "LeBool") [],
              MyConstructor (mkTyCon "Vrai")
            )
      it "type Nat = Zero | Suc Nat in Suc Zero" $ do
        result <- eval stdLib "type Nat = Zero | Suc Nat in Suc Zero"
        result
          `shouldBe` Right
            ( MTData (mkTyCon "Nat") [],
              MyConsApp (MyConstructor (mkTyCon "Suc")) (MyConstructor (mkTyCon "Zero"))
            )
      it "type Nat = Zero | Suc Nat in Suc Suc Zero" $ do
        result <- eval stdLib "type Nat = Zero | Suc Nat in Suc Suc Zero"
        result
          `shouldBe` Right
            ( MTData (mkTyCon "Nat") [],
              MyConsApp
                (MyConstructor (mkTyCon "Suc"))
                ( MyConsApp (MyConstructor (mkTyCon "Suc")) (MyConstructor (mkTyCon "Zero"))
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
                (MTData (mkTyCon "Nat") [])
                (MTData (mkTyCon "Nat") []),
              MyConstructor (mkTyCon "Suc")
            )
      it "type OhNat = Zero | Suc OhNat String in Suc" $ do
        result <- eval stdLib "type OhNat = Zero | Suc OhNat String in Suc"
        result
          `shouldBe` Right
            ( MTFunction
                (MTData (mkTyCon "OhNat") [])
                ( MTFunction
                    MTString
                    (MTData (mkTyCon "OhNat") [])
                ),
              MyConstructor (mkTyCon "Suc")
            )
      it "type Pet = Cat String | Dog String in Cat \"mimsa\"" $ do
        result <- eval stdLib "type Pet = Cat String | Dog String in Cat \"mimsa\""
        result
          `shouldBe` Right
            ( MTData (mkTyCon "Pet") [],
              MyConsApp (MyConstructor (mkTyCon "Cat")) (str' "mimsa")
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
                    (MTData (mkTyCon "LongBoy") [])
                ),
              MyConsApp
                (MyConstructor (mkTyCon "Stuff"))
                (str' "yes")
            )
      it "type Tree = Leaf Int | Branch Tree Tree in Branch (Leaf 1) (Leaf 2)" $ do
        result <- eval stdLib "type Tree = Leaf Int | Branch Tree Tree in Branch (Leaf 1) (Leaf 2)"
        result
          `shouldBe` Right
            ( MTData (mkTyCon "Tree") [],
              MyConsApp
                ( MyConsApp
                    (MyConstructor $ mkTyCon "Branch")
                    (MyConsApp (MyConstructor $ mkTyCon "Leaf") (int 1))
                )
                (MyConsApp (MyConstructor $ mkTyCon "Leaf") (int 2))
            )
      it "type Maybe a = Just a | Nothing in Just" $ do
        result <- eval stdLib "type Maybe a = Just a | Nothing in Just"
        result
          `shouldBe` Right
            ( MTFunction
                (MTVar (NumberedVar 1))
                (MTData (mkTyCon "Maybe") [MTVar (NumberedVar 1)]),
              MyConstructor $ mkTyCon "Just"
            )
      it "type Maybe a = Just a | Nothing in Nothing" $ do
        result <- eval stdLib "type Maybe a = Just a | Nothing in Nothing"
        result
          `shouldBe` Right
            ( MTData (mkTyCon "Maybe") [MTVar (NumberedVar 1)],
              MyConstructor $ mkTyCon "Nothing"
            )
      it "type Maybe a = Just a | Nothing in Just 1" $ do
        result <- eval stdLib "type Maybe a = Just a | Nothing in Just 1"
        result
          `shouldBe` Right
            ( MTData (mkTyCon "Maybe") [MTInt],
              MyConsApp (MyConstructor $ mkTyCon "Just") (int 1)
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
            ( MTData (mkTyCon "Tree") [MTInt],
              MyConsApp (MyConstructor $ mkTyCon "Leaf") (int 1)
            )
      it "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Leaf 1" $ do
        result <- eval stdLib "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Leaf 1"
        result
          `shouldSatisfy` isLeft
      it "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Branch (Leaf 1) (Leaf True)" $ do
        result <- eval stdLib "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Branch (Leaf 1) (Leaf True)"
        result
          `shouldSatisfy` isLeft
      it "type Tree a = Empty | Branch (Tree a) a (Tree a) in Branch (Empty) 1 (Empty)" $ do
        result <- eval stdLib "type Tree a = Empty | Branch (Tree a) a (Tree a) in Branch (Empty) 1 (Empty)"
        result
          `shouldBe` Right
            ( MTData (mkTyCon "Tree") [MTInt],
              MyConsApp
                ( MyConsApp
                    ( MyConsApp
                        (MyConstructor $ mkTyCon "Branch")
                        (MyConstructor $ mkTyCon "Empty")
                    )
                    (int 1)
                )
                (MyConstructor $ mkTyCon "Empty")
            )
      it "type Maybe a = Just a | Nothing in case Just True of Just \\a -> a | Nothing \"what\"" $ do
        result <- eval stdLib "type Maybe a = Just a | Nothing in case Just True of Just \\a -> a | Nothing \"what\""
        result `shouldSatisfy` isLeft
      it "type Either e a = Left e | Right a in \\f -> \\g -> \\either -> case either of Left \\e -> g(e) | Right \\a -> f(a)" $ do
        result <- eval stdLib "type Either e a = Left e | Right a in \\f -> \\g -> \\either -> case either of Left \\e -> g(e) | Right \\a -> f(a)"
        result `shouldSatisfy` isRight
      {-
            it "type Maybe a = Just a | Nothing in \\maybe -> case maybe of Just \\a -> a | Nothing \"poo\"" $ do
              result <- eval stdLib "type Maybe a = Just a | Nothing in \\maybe -> case maybe of Just \\a -> a | Nothing \"poo\""
              fst <$> result
                `shouldBe` Right
                  ( MTFunction (MTData (mkTyCon "Maybe") []) MTString
                  )
      
      -}
      it "type Arr a = Empty | Item a (Arr a) in case (Item 1 (Item 2 Empty)) of Empty Empty | Item \\a -> \\rest -> rest" $ do
        result <- eval stdLib "type Arr a = Empty | Item a (Arr a) in case (Item 1 (Item 2 Empty)) of Empty Empty | Item \\a -> \\rest -> rest"
        result
          `shouldBe` Right
            ( MTData (mkTyCon "Arr") [MTInt],
              MyConsApp
                ( MyConsApp
                    ( MyConstructor
                        (mkTyCon "Item")
                    )
                    (int 2)
                )
                (MyConstructor $ mkTyCon "Empty")
            )
      it "let loop = (\\a -> if eq(10)(a) then a else loop(addInt(a)(1))) in loop(1)" $ do
        result <- eval stdLib "let loop = (\\a -> if eq(10)(a) then a else loop(addInt(a)(1))) in loop(1)"
        result `shouldBe` Right (MTInt, int 10)
      it "type Nat = Zero | Suc Nat in let loop = (\\as -> case as of Zero 0 | Suc \\as2 -> incrementInt(loop(as2))) in loop(Suc Suc Suc Zero)" $ do
        result <- eval stdLib "type Nat = Zero | Suc Nat in let loop = (\\as -> case as of Zero 0 | Suc \\as2 -> incrementInt(loop(as2))) in loop(Suc Suc Suc Zero)"
        result `shouldBe` Right (MTInt, int 3)
      it "type Nat = Zero | Suc Nat in let loop = (\\as -> \\b -> case as of Zero b | Suc \\as2 -> incrementInt(loop(as2)(b))) in loop(Suc Suc Suc Zero)(10)" $ do
        result <- eval stdLib "type Nat = Zero | Suc Nat in let loop = (\\as -> \\b -> case as of Zero b | Suc \\as2 -> incrementInt(loop(as2)(b))) in loop(Suc Suc Suc Zero)(10)"
        result `shouldBe` Right (MTInt, int 13)
      {-
            it "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\b -> \\as -> case as of Empty b | Item \\a -> \\rest -> reduceA(addInt(b)(a))(rest)) in reduceA(0)(Item 3 Empty)" $ do
              result <- eval stdLib "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\b -> \\as -> case as of Empty b | Item \\a -> \\rest -> reduceA(addInt(b)(a))(rest)) in reduceA(0)(Item 3 Empty)"
              result `shouldBe` Right (MTInt, int 3)
      -}
      it "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\f -> \\b -> \\as -> case as of Empty b | Item \\a -> \\rest -> reduceA(f)(f(b)(a))(rest)) in reduceA(addInt)(0)(Empty)" $ do
        result <- eval stdLib "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\f -> \\b -> \\as -> case as of Empty b | Item \\a -> \\rest -> reduceA(f)(f(b)(a))(rest)) in reduceA(addInt)(0)(Empty)"
        result `shouldBe` Right (MTInt, int 0)
      it "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\f -> \\b -> \\as -> case as of Empty b | Item \\a -> \\rest -> reduceA(f)(f(b)(a))(rest)) in reduceA(addInt)(0)(Item 3 Empty)" $ do
        result <- eval stdLib "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\f -> \\b -> \\as -> case as of Empty b | Item \\a -> \\rest -> reduceA(f)(f(b)(a))(rest)) in reduceA(addInt)(0)(Item 3 Empty)"
        result `shouldBe` Right (MTInt, int 3)
      it "let some = \\a -> Some a in if True then some(1) else Nowt" $ do
        result <- eval stdLib "let some = \\a -> Some a in if True then some(1) else Nowt"
        result
          `shouldBe` Right
            ( MTData (mkTyCon "Option") [MTInt],
              MyConsApp
                (MyConstructor (mkTyCon "Some"))
                (int 1)
            )
      it "\\a -> case a of Some \\as -> True | Nowt 100" $ do
        result <- eval stdLib "\\a -> case a of Some \\as -> True | Nowt 100"
        fst <$> result
          `shouldSatisfy` isLeft
      it "\\a -> case a of Some \\as -> as | Nowt 100" $ do
        result <- eval stdLib "\\a -> case a of Some \\as -> as | Nowt 100"
        fst <$> result
          `shouldBe` Right
            (MTFunction (MTData (mkTyCon "Option") [MTInt]) MTInt)
      it "let fromMaybe = \\def -> (\\maybe -> case maybe of Some (\\a -> a) | Nowt def) in fromMaybe(\"Horse\")(Some 1)" $ do
        result <- eval stdLib "let fromMaybe = \\def -> (\\maybe -> case maybe of Some (\\a -> a) | Nowt def) in fromMaybe(\"Horse\")(Some 1)"
        result `shouldSatisfy` isLeft
      it "let fromMaybe = \\def -> (\\maybe -> case maybe of Some (\\a -> a) | Nowt def) in fromMaybe(\"Horse\")(Some \"Dog\")" $ do
        result <- eval stdLib "let fromMaybe = \\def -> (\\maybe -> case maybe of Some (\\a -> a) | Nowt def) in fromMaybe(\"Horse\")(Some \"Dog\")"
        result `shouldBe` Right (MTString, str' "Dog")
