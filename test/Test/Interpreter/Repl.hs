{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Interpreter.Repl
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import Data.Functor (($>))
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.ExprUtils
import Language.Mimsa.Interpreter
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Repl
import Language.Mimsa.Store.Hashing
import Language.Mimsa.Store.Storage (getStoreExpressionHash)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers
import Test.Utils.Serialisation
  ( createOutputFolder,
    saveJSON,
    savePretty,
  )

eval ::
  Project Annotation ->
  Text ->
  IO (Either Text (Type (), Expr Variable ()))
eval env input =
  case evaluateText env input of
    Left e -> pure (Left $ prettyPrint e)
    Right (ResolvedExpression mt se expr' scope' swaps) -> do
      saveRegressionData (se $> ())
      let endExpr = interpret scope' swaps expr'
      case toEmptyAnn <$> endExpr of
        Right a -> pure (Right (toEmptyType mt, a))
        Left e -> pure (Left (prettyPrint $ InterpreterErr e))

-- These are saved and used in the deserialisation tests to make sure we avoid
-- future regressions
saveRegressionData :: StoreExpression () -> IO ()
saveRegressionData se = do
  jsonPath <- createOutputFolder "StoreExpr"
  let jsonFilename = jsonPath <> show (getStoreExpressionHash se) <> ".json"
  saveJSON jsonFilename se
  prettyPath <- createOutputFolder "PrettyPrint"
  let prettyFilename = prettyPath <> show (getStoreExpressionHash se) <> ".mimsa"
  savePretty prettyFilename (storeExpression se)

saveProject :: Project ann -> IO ()
saveProject prj = do
  let saveProject' = projectToSaved prj
  let (_, projectHash) = contentAndHash saveProject'
  jsonPath <- createOutputFolder "SaveProject"
  let jsonFilename = jsonPath <> show projectHash <> ".json"
  saveJSON jsonFilename saveProject'

-- remove annotations for comparison
toEmptyAnn :: Expr a b -> Expr a ()
toEmptyAnn = toEmptyAnnotation

toEmptyType :: Type a -> Type ()
toEmptyType a = a $> ()

spec :: Spec
spec =
  describe "Repl" $ do
    it "Save stdLib" $
      saveProject stdLib
        >> (True `shouldBe` True)
    describe
      "End to end parsing to evaluation"
      $ do
        it "let x = ((1,2)) in fst(x)" $ do
          result <- eval stdLib "let x = ((1,2)) in fst(x)"
          result
            `shouldBe` Right
              (MTPrim mempty MTInt, int 1)
        it "let good = { dog: True } in good.dog" $ do
          result <- eval stdLib "let good = ({ dog: True }) in good.dog"
          result `shouldBe` Right (MTPrim mempty MTBool, bool True)
        it "let prelude = { id: (\\i -> i) } in prelude.id" $ do
          result <- eval stdLib "let prelude = ({ id: (\\i -> i) }) in prelude.id"
          result
            `shouldBe` Right
              ( MTFunction mempty (unknown 1) (unknown 1),
                MyLambda mempty (numbered 3) (MyVar mempty (numbered 3))
              )
        it "let prelude = ({ id: (\\i -> i) }) in prelude.id(1)" $ do
          result <- eval stdLib "let prelude = ({ id: (\\i -> i) }) in prelude.id(1)"
          result
            `shouldBe` Right
              ( MTPrim mempty MTInt,
                int 1
              )
        it "let bigPrelude = ({ prelude: { id: (\\i -> i) } }) in bigPrelude.prelude.id(1)" $ do
          result <- eval stdLib "let bigPrelude = ({ prelude: { id: (\\i -> i) } }) in bigPrelude.prelude.id(1)"
          result
            `shouldBe` Right
              ( MTPrim mempty MTInt,
                int 1
              )
        it "let compose = (\\f -> \\g -> \\a -> f(g(a))) in compose(incrementInt)(incrementInt)(67)" $ do
          result <- eval stdLib "let compose = (\\f -> \\g -> \\a -> f(g(a))) in compose(incrementInt)(incrementInt)(67)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 69)
        it "let reuse = ({ first: id(1), second: id(2) }) in reuse.first" $ do
          result <- eval stdLib "let reuse = ({ first: id(1), second: id(2) }) in reuse.first"
          result `shouldBe` Right (MTPrim mempty MTInt, int 1)
        it "let id = \\a -> a in id(1)" $ do
          result <- eval mempty "let id = \\a -> a in id(1)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 1)
        it "let reuse = ({ first: id(True), second: id(2) }) in reuse.first" $ do
          result <- eval stdLib "let reuse = ({ first: id(True), second: id(2) }) in reuse.first"
          result `shouldBe` Right (MTPrim mempty MTBool, bool True)
        it "let reuse = ({ first: id, second: id(2) }) in reuse.first(True)" $ do
          result <- eval stdLib "let reuse = ({ first: id, second: id(2) }) in reuse.first(True)"
          result `shouldBe` Right (MTPrim mempty MTBool, bool True)
        it "let const2 = \\a -> \\b -> a in (let reuse = ({ first: const2(1), second: const2(True) }) in reuse.first(100))" $ do
          result <- eval stdLib "let const2 = \\a -> \\b -> a in (let reuse = ({ first: const2(1), second: const2(True) }) in reuse.first(100))"
          result `shouldBe` Right (MTPrim mempty MTInt, int 1)
        it "let const2 = \\a -> \\b -> a in (let reuse = ({ first: const2(True), second: const2(2) }) in reuse.second(100))" $ do
          result <- eval stdLib "let const2 = \\a -> \\b -> a in (let reuse = ({ first: const2(True), second: const2(2) }) in reuse.second(100))"
          result `shouldBe` Right (MTPrim mempty MTInt, int 2)
        it "addInt(1)(2)" $ do
          result <- eval stdLib "addInt(1)(2)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 3)
        it "(\\a -> a)(1)" $ do
          result <- eval stdLib "(\\a -> a)(1)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 1)
        it "(\\b -> (\\a -> b))(0)(1)" $ do
          result <- eval stdLib "(\\b -> (\\a -> b))(0)(1)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 0)
        it "addInt(1)(addInt(addInt(2)(4))(5))" $ do
          result <- eval stdLib "addInt(1)(addInt(addInt(2)(4))(5))"
          result `shouldBe` Right (MTPrim mempty MTInt, int 12)
        it "type LeBool = Vrai | Faux in Vrai" $ do
          result <- eval stdLib "type LeBool = Vrai | Faux in Vrai"
          result
            `shouldBe` Right
              ( MTData mempty "LeBool" [],
                MyConstructor mempty "Vrai"
              )
        it "type Nat = Zero | Suc Nat in Suc Zero" $ do
          result <- eval stdLib "type Nat = Zero | Suc Nat in Suc Zero"
          result
            `shouldBe` Right
              ( MTData mempty "Nat" [],
                MyConsApp
                  mempty
                  (MyConstructor mempty "Suc")
                  (MyConstructor mempty "Zero")
              )
        it "type Nat = Zero | Suc Nat in Suc Suc Zero" $ do
          result <- eval stdLib "type Nat = Zero | Suc Nat in Suc Suc Zero"
          result
            `shouldBe` Right
              ( MTData mempty "Nat" [],
                MyConsApp
                  mempty
                  (MyConstructor mempty "Suc")
                  ( MyConsApp
                      mempty
                      (MyConstructor mempty "Suc")
                      (MyConstructor mempty "Zero")
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
                  mempty
                  (MTData mempty "Nat" [])
                  (MTData mempty "Nat" []),
                MyConstructor mempty "Suc"
              )
        it "type OhNat = Zero | Suc OhNat String in Suc" $ do
          result <- eval stdLib "type OhNat = Zero | Suc OhNat String in Suc"
          result
            `shouldBe` Right
              ( MTFunction
                  mempty
                  (MTData mempty "OhNat" [])
                  ( MTFunction
                      mempty
                      (MTPrim mempty MTString)
                      (MTData mempty "OhNat" [])
                  ),
                MyConstructor mempty "Suc"
              )
        it "type Pet = Cat String | Dog String in Cat \"mimsa\"" $ do
          result <- eval stdLib "type Pet = Cat String | Dog String in Cat \"mimsa\""
          result
            `shouldBe` Right
              ( MTData mempty "Pet" [],
                MyConsApp
                  mempty
                  (MyConstructor mempty "Cat")
                  (str' "mimsa")
              )
        it "type Void in 1" $ do
          result <- eval stdLib "type Void in 1"
          result `shouldBe` Right (MTPrim mempty MTInt, int 1)
        it "type String = Should | Error in Error" $ do
          result <- eval stdLib "type String = Should | Error in Error"
          result `shouldSatisfy` isLeft
        it "type LongBoy = Stuff String Int String in Stuff \"yes\"" $ do
          result <- eval stdLib "type LongBoy = Stuff String Int String in Stuff \"yes\""
          result
            `shouldBe` Right
              ( MTFunction
                  mempty
                  (MTPrim mempty MTInt)
                  ( MTFunction
                      mempty
                      (MTPrim mempty MTString)
                      (MTData mempty "LongBoy" [])
                  ),
                MyConsApp
                  mempty
                  (MyConstructor mempty "Stuff")
                  (str' "yes")
              )
        it "type Tree = Leaf Int | Branch Tree Tree in Branch (Leaf 1) (Leaf 2)" $ do
          result <- eval stdLib "type Tree = Leaf Int | Branch Tree Tree in Branch (Leaf 1) (Leaf 2)"
          result
            `shouldBe` Right
              ( MTData mempty "Tree" [],
                MyConsApp
                  mempty
                  ( MyConsApp
                      mempty
                      (MyConstructor mempty "Branch")
                      (MyConsApp mempty (MyConstructor mempty "Leaf") (int 1))
                  )
                  (MyConsApp mempty (MyConstructor mempty "Leaf") (int 2))
              )
        it "type Maybe a = Just a | Nothing in Just" $ do
          result <- eval stdLib "type Maybe a = Just a | Nothing in Just"
          result
            `shouldBe` Right
              ( MTFunction
                  mempty
                  (MTVar mempty (tvNumbered 1))
                  (MTData mempty "Maybe" [MTVar mempty (tvNumbered 1)]),
                MyConstructor mempty "Just"
              )
        it "type Maybe a = Just a | Nothing in Nothing" $ do
          result <- eval stdLib "type Maybe a = Just a | Nothing in Nothing"
          result
            `shouldBe` Right
              ( MTData mempty "Maybe" [MTVar mempty (tvNumbered 1)],
                MyConstructor mempty "Nothing"
              )
        it "type Maybe a = Just a | Nothing in Just 1" $ do
          result <- eval stdLib "type Maybe a = Just a | Nothing in Just 1"
          result
            `shouldBe` Right
              ( MTData mempty "Maybe" [MTPrim mempty MTInt],
                MyConsApp
                  mempty
                  (MyConstructor mempty "Just")
                  (int 1)
              )
        it "type Maybe a = Just a | Nothing in case Just 1 of Just \\a -> eq(100)(a) | Nothing False" $ do
          result <- eval stdLib "type Maybe a = Just a | Nothing in case Just 1 of Just \\a -> eq(100)(a) | Nothing False"
          result
            `shouldBe` Right
              (MTPrim mempty MTBool, bool False)
        it "type Maybe a = Just a | Nothing in case Just 1 of Just \\a -> True | Nothing 1" $ do
          result <- eval stdLib "type Maybe a = Just a | Nothing in case Just 1 of Just \\a -> True | Nothing 1"
          result `shouldSatisfy` isLeft
        it "type Maybe a = Just a | Nothing in case Just 1 of Just \\a -> eq(100)(a) | otherwise False" $ do
          result <- eval stdLib "type Maybe a = Just a | Nothing in case Just 1 of Just \\a -> eq(100)(a) | otherwise False"
          result
            `shouldBe` Right
              (MTPrim mempty MTBool, bool False)
        it "type Stuff = Thing String Int in case Thing \"Hello\" 1 of Thing \\name -> \\num -> name" $ do
          result <- eval stdLib "type Stuff = Thing String Int in case Thing \"Hello\" 1 of Thing \\name -> \\num -> name"
          result
            `shouldBe` Right
              (MTPrim mempty MTString, str' "Hello")
        it "type Result e a = Failure e | Success a in case Failure \"oh no\" of Success \\a -> \"oh yes\" | Failure \\e -> e" $ do
          result <- eval stdLib "type Result e a = Failure e | Success a in case Failure \"oh no\" of Success \\a -> \"oh yes\" | Failure \\e -> e"
          result
            `shouldBe` Right
              (MTPrim mempty MTString, str' "oh no")
        it "type Blap a = Boop a Int in case Boop True 100 of Boop \\a -> \\b -> a" $ do
          result <- eval stdLib "type Blap a = Boop a Int in case Boop True 100 of Boop \\a -> \\b -> a"
          result `shouldBe` Right (MTPrim mempty MTBool, bool True)
        it "type Maybe a = Just a | Nothing in case Nothing of Nothing False" $ do
          result <- eval stdLib "type Maybe a = Just a | Nothing in case Nothing of Nothing False"
          result `shouldSatisfy` isLeft
        it "type Thing = Thing String in let a = Thing \"string\" in case a of Thing \\s -> s" $ do
          result <- eval stdLib "type Thing = Thing String in let a = Thing \"string\" in case a of Thing \\s -> s"
          result `shouldBe` Right (MTPrim mempty MTString, str' "string")
        it "type Pair a b = Pair a b in case Pair \"dog\" 1 of Pair \a -> a" $ do
          result <- eval stdLib "type Pair a b = Pair a b in case Pair \"dog\" 1 of Pair \a -> a"
          result `shouldSatisfy` isLeft
        it "type Tree a = Leaf a | Branch (Tree a) (Tree a) in Leaf 1" $ do
          result <- eval stdLib "type Tree a = Leaf a | Branch (Tree a) (Tree a) in Leaf 1"
          result
            `shouldBe` Right
              ( MTData mempty "Tree" [MTPrim mempty MTInt],
                MyConsApp
                  mempty
                  (MyConstructor mempty "Leaf")
                  (int 1)
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
              ( MTData mempty "Tree" [MTPrim mempty MTInt],
                MyConsApp
                  mempty
                  ( MyConsApp
                      mempty
                      ( MyConsApp
                          mempty
                          (MyConstructor mempty "Branch")
                          (MyConstructor mempty "Empty")
                      )
                      (int 1)
                  )
                  (MyConstructor mempty "Empty")
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
                    ( MTFunction (MTData ( "Maybe") []) (MTPrim MTString)
                    )

        -}
        it "type Arr a = Empty | Item a (Arr a) in case (Item 1 (Item 2 Empty)) of Empty Empty | Item \\a -> \\rest -> rest" $ do
          result <- eval stdLib "type Arr a = Empty | Item a (Arr a) in case (Item 1 (Item 2 Empty)) of Empty Empty | Item \\a -> \\rest -> rest"
          result
            `shouldBe` Right
              ( MTData mempty "Arr" [MTPrim mempty MTInt],
                MyConsApp
                  mempty
                  ( MyConsApp
                      mempty
                      ( MyConstructor
                          mempty
                          "Item"
                      )
                      (int 2)
                  )
                  (MyConstructor mempty "Empty")
              )
        it "let loop = (\\a -> if eq(10)(a) then a else loop(addInt(a)(1))) in loop(1)" $ do
          result <- eval stdLib "let loop = (\\a -> if eq(10)(a) then a else loop(addInt(a)(1))) in loop(1)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 10)
        it "type Nat = Zero | Suc Nat in let loop = (\\as -> case as of Zero 0 | Suc \\as2 -> incrementInt(loop(as2))) in loop(Suc Suc Suc Zero)" $ do
          result <- eval stdLib "type Nat = Zero | Suc Nat in let loop = (\\as -> case as of Zero 0 | Suc \\as2 -> incrementInt(loop(as2))) in loop(Suc Suc Suc Zero)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 3)
        it "type Nat = Zero | Suc Nat in let loop = (\\as -> \\b -> case as of Zero b | Suc \\as2 -> incrementInt(loop(as2)(b))) in loop(Suc Suc Suc Zero)(10)" $ do
          result <- eval stdLib "type Nat = Zero | Suc Nat in let loop = (\\as -> \\b -> case as of Zero b | Suc \\as2 -> incrementInt(loop(as2)(b))) in loop(Suc Suc Suc Zero)(10)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 13)
        {-
              it "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\b -> \\as -> case as of Empty b | Item \\a -> \\rest -> reduceA(addInt(b)(a))(rest)) in reduceA(0)(Item 3 Empty)" $ do
                result <- eval stdLib "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\b -> \\as -> case as of Empty b | Item \\a -> \\rest -> reduceA(addInt(b)(a))(rest)) in reduceA(0)(Item 3 Empty)"
                result `shouldBe` Right (MTPrim mempty MTInt, int 3)
        -}
        it "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\f -> \\b -> \\as -> case as of Empty b | Item \\a -> \\rest -> reduceA(f)(f(b)(a))(rest)) in reduceA(addInt)(0)(Empty)" $ do
          result <- eval stdLib "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\f -> \\b -> \\as -> case as of Empty b | Item \\a -> \\rest -> reduceA(f)(f(b)(a))(rest)) in reduceA(addInt)(0)(Empty)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 0)
        it "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\f -> \\b -> \\as -> case as of Empty b | Item \\a -> \\rest -> reduceA(f)(f(b)(a))(rest)) in reduceA(addInt)(0)(Item 3 Empty)" $ do
          result <- eval stdLib "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\f -> \\b -> \\as -> case as of Empty b | Item \\a -> \\rest -> reduceA(f)(f(b)(a))(rest)) in reduceA(addInt)(0)(Item 3 Empty)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 3)
        it "type Tlee a = Non | Tlee (Option b) in {}" $ do
          result <- eval stdLib "type Tlee a = Non | Tlee (Option b) in {}"
          result `shouldSatisfy` isLeft
        it "let some = \\a -> Some a in if True then some(1) else Nowt" $ do
          result <- eval stdLib "let some = \\a -> Some a in if True then some(1) else Nowt"
          result
            `shouldBe` Right
              ( MTData mempty "Option" [MTPrim mempty MTInt],
                MyConsApp
                  mempty
                  (MyConstructor mempty "Some")
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
              ( MTFunction
                  mempty
                  (MTData mempty "Option" [MTPrim mempty MTInt])
                  (MTPrim mempty MTInt)
              )
        it "let fromMaybe = \\def -> (\\maybe -> case maybe of Some (\\a -> a) | Nowt def) in fromMaybe(\"Horse\")(Some 1)" $ do
          result <- eval stdLib "let fromMaybe = \\def -> (\\maybe -> case maybe of Some (\\a -> a) | Nowt def) in fromMaybe(\"Horse\")(Some 1)"
          result `shouldSatisfy` isLeft
        it "let fromMaybe = \\def -> (\\maybe -> case maybe of Some (\\a -> a) | Nowt def) in fromMaybe(\"Horse\")(Some \"Dog\")" $ do
          result <- eval stdLib "let fromMaybe = \\def -> (\\maybe -> case maybe of Some (\\a -> a) | Nowt def) in fromMaybe(\"Horse\")(Some \"Dog\")"
          result `shouldBe` Right (MTPrim mempty MTString, str' "Dog")
        it "True == \"dog\"" $ do
          result <- eval stdLib "True == \"dog\""
          result `shouldSatisfy` isLeft
        it "(\\a -> a) == (\\b -> b)" $ do
          -- no function equality
          result <- eval stdLib "(\\a -> a) == (\\b -> b)"
          result `shouldSatisfy` isLeft
        it "True == False" $ do
          result <- eval stdLib "True == False"
          result `shouldBe` Right (MTPrim mempty MTBool, bool False)
        it "True == True" $ do
          result <- eval stdLib "True == True"
          result `shouldBe` Right (MTPrim mempty MTBool, bool True)
        it "(Some 1) == Some 2" $ do
          result <- eval stdLib "(Some 1) == Some 2"
          result `shouldBe` Right (MTPrim mempty MTBool, bool False)
        it "let eq1 = (\\a -> a == 1) in eq1(1)" $ do
          result <- eval stdLib "let eq1 = (\\a -> a == 1) in eq1(1)"
          result `shouldBe` Right (MTPrim mempty MTBool, bool True)
        it "1 + 1" $ do
          result <- eval stdLib "1 + 1"
          result `shouldBe` Right (MTPrim mempty MTInt, int 2)
        it "True + 1" $ do
          result <- eval stdLib "True + 1"
          result `shouldSatisfy` isLeft
        it "10 - 1" $ do
          result <- eval stdLib "10 - 1"
          result `shouldBe` Right (MTPrim mempty MTInt, int 9)
        it "True - 1" $ do
          result <- eval stdLib "True - 1"
          result `shouldSatisfy` isLeft
        it "1 + 1 + 1 + 1" $ do
          result <- eval stdLib "1 + 1 + 1 + 1"
          result `shouldBe` Right (MTPrim mempty MTInt, int 4)
        it "\"dog\" <> \"log\"" $ do
          result <- eval stdLib "\"dog\" <> \"log\""
          result `shouldBe` Right (MTPrim mempty MTString, str' "doglog")
        it "\"dog\" <> 123" $ do
          result <- eval stdLib "\"dog\" <> 123"
          result `shouldSatisfy` isLeft
        it "let f = (\\a -> if True then a.num else a.num2) in f({num: 1, num2: 2})" $ do
          result <- eval stdLib "let f = (\\a -> if True then a.num else a.num2) in f({num: 1, num2: 2})"
          result `shouldBe` Right (MTPrim mempty MTInt, int 1)
        it "if True then { one: 1 } else { two: 2 }" $ do
          result <- eval stdLib "if True then { one: 1 } else { two: 2 }"
          result `shouldSatisfy` isLeft
        it "if True then { one: 1 } else { one: 2 }" $ do
          result <- eval stdLib "if True then { one: 1 } else { one: 2 }"
          result `shouldSatisfy` isRight
        it "let a = { one: 1 }; let one = a.one; let two = a.two; a" $ do
          result <- eval stdLib "let a = { one: 1 }; let one = a.one; let two = a.two; a"
          result `shouldSatisfy` isLeft
        it "\\a -> let one = a.one; let two = a.two; a" $ do
          result <- eval stdLib "\\a -> let one = a.one; let two = a.two; a"
          result
            `shouldSatisfy` isRight
        it "let useRecord = (\\a -> let one = a.one; let two = a.two; one + two) in useRecord({one: 1})" $ do
          result <- eval stdLib "let useRecord = (\\a -> let one = a.one; let two = a.two; one + two) in useRecord({one: 1})"
          result `shouldSatisfy` \case
            (Left err) -> not $ T.isInfixOf "InterpreterError" err
            _ -> False
        it "let useRecord = (\\a -> let one = a.one; let two = a.two; one + two) in useRecord({two: 2})" $ do
          result <- eval stdLib "let useRecord = (\\a -> let one = a.one; let two = a.two; one + two) in useRecord({two: 2})"
          result `shouldSatisfy` \case
            (Left err) -> not $ T.isInfixOf "InterpreterError" err
            _ -> False
        it "let useRecord = (\\a -> let one = a.one; let two = a.two; one + two) in useRecord({one: 1, two: 2})" $ do
          result <- eval stdLib "let useRecord = (\\a -> let one = a.one; let two = a.two; one + two) in useRecord({one: 1, two: 2})"
          result `shouldSatisfy` isRight
        it "\\a -> let one = a.one; \\a -> let two = a.two in a.one" $ do
          result <- eval stdLib "\\a -> let one = a.one; \\a -> let two = a.two in a.one"
          -- here the two a's should be different types due to shadowing
          -- but even knowing the second a is different it can infer stuff
          fst <$> result
            `shouldBe` Right
              ( MTFunction
                  mempty
                  ( MTRecord
                      mempty
                      (M.singleton "one" (MTVar mempty (tvFree 1)))
                  )
                  ( MTFunction
                      mempty
                      ( MTRecord
                          mempty
                          ( M.fromList
                              [ ("one", MTVar mempty (tvFree 2)),
                                ("two", MTVar mempty (tvFree 3))
                              ]
                          )
                      )
                      (MTVar mempty (tvFree 2))
                  )
              )
        it "if ?missingFn then 1 else 2" $ do
          result <- eval stdLib "if ?missingFn then 1 else 2"
          result `shouldSatisfy` \case
            (Left msg) ->
              T.isInfixOf "Typed holes found" msg
                && T.isInfixOf "?missingFn" msg
                && T.isInfixOf "Boolean" msg
            (Right _) -> False
        it "let map = \\f -> \\a -> f(a) in map(?flappy)(1)" $ do
          result <- eval stdLib "let map = \\f -> \\a -> f(a) in map(?flappy)(1)"
          result `shouldSatisfy` \case
            (Left msg) ->
              T.isInfixOf "Typed holes found" msg
                && T.isInfixOf "^^^^^^^" msg
            (Right _) -> False
        it "let compose = \\f -> \\g -> \\a -> f(g(a)); compose" $ do
          result <- eval stdLib "let compose = \\f -> \\g -> \\a -> f(g(a)); compose"
          result `shouldSatisfy` isRight
        it "Some (1 == 1)" $ do
          result <- eval stdLib "Some (1 == 1)"
          snd <$> result
            `shouldBe` Right
              ( MyConsApp
                  mempty
                  (MyConstructor mempty "Some")
                  (bool True)
              )
        it "\\a -> if (100 == a.int) then 100 else 0" $ do
          result <- eval stdLib "\\a -> if (100 == a.int) then 100 else 0"
          result `shouldSatisfy` isRight
        it "\\a -> if (a.one == a.two) then 100 else 0" $ do
          result <- eval stdLib "\\a -> if (a.one == a.two) then 100 else 0"
          result `shouldSatisfy` isRight
        it "type Reader r a = Reader (r -> a) in Reader \\r -> r + 100" $ do
          result <- eval stdLib "type Reader r a = Reader (r -> a) in Reader \\r -> r + 100"
          result
            `shouldBe` Right
              ( MTData
                  mempty
                  "Reader"
                  [MTPrim mempty MTInt, MTPrim mempty MTInt],
                MyConsApp
                  mempty
                  (MyConstructor mempty "Reader")
                  ( MyLambda
                      mempty
                      (numbered 0)
                      ( MyInfix
                          mempty
                          Add
                          (MyVar mempty (numbered 0))
                          (int 100)
                      )
                  )
              )
        it "\\state -> \\s -> case state of State \\sas -> sas(s)" $ do
          result <- eval stdLib "\\state -> \\s -> case state of State \\sas -> sas(s)"
          result `shouldSatisfy` isRight
        it "let a = pureState(\"dog\"); let b = bindState(storeName)(a); runState(b)(nil)" $ do
          result <- eval stdLib "let a = pureState(\"dog\"); let b = bindState(storeName)(a); runState(b)(nil)"
          result `shouldSatisfy` isRight
        it "let a = pureState(\"dog\"); let b = bindState(storeName)(a); let c = bindState(storeName)(b); runState(c)(nil)" $ do
          result <- eval stdLib "let a = pureState(\"dog\"); let b = bindState(storeName)(a); let c = bindState(storeName)(b); runState(c)(nil)"
          result `shouldSatisfy` isRight
        it "infix <<< = compose; True" $ do
          result <- eval stdLib "infix <<< = compose; True"
          -- binding to a two arity function is A++
          result `shouldBe` Right (MTPrim mempty MTBool, bool True)
        it "infix <<< = incrementInt; True" $ do
          result <- eval stdLib "infix <<< = incrementInt; True"
          -- we can only bind to a two arity function
          result `shouldSatisfy` isLeft
        it "infix <<< = id; True" $ do
          result <- eval stdLib "infix <<< = id; True"
          -- we check polymorphic functions
          result `shouldSatisfy` isLeft
        it "infix +++ = addInt; 1 +++ 2" $ do
          result <- eval stdLib "infix +++ = addInt; 1 +++ 2"
          result `shouldBe` Right (MTPrim mempty MTInt, int 3)
        it "addInt(1)(2)" $ do
          result <- eval stdLib "addInt(1)(2)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 3)
        it "infix == = addInt; True" $ do
          result <- eval stdLib "infix == = addInt; True"
          -- can't overwrite built in infix operators
          result `shouldSatisfy` isLeft
        it "infix +++ = addInt; 1 +++ True" $ do
          result <- eval stdLib "infix +++ = addInt; 1 +++ True"
          -- function typechecking should still work
          result `shouldSatisfy` isLeft
        it "\\some -> case some of Some \\a -> Some (a == 1) | otherwise some" $ do
          result <- eval stdLib "\\some -> case some of Some \\a -> Some (a == 1) | otherwise some"
          result `shouldSatisfy` isLeft
        -- this should be thrown out by the interpreter
        it "let forever = \\a -> forever(a) in forever(True)" $ do
          result <- eval stdLib "let forever = \\a -> forever(a) in forever(True)"
          result `shouldSatisfy` \case
            Left msg -> "interpreter aborted" `T.isInfixOf` msg
            _ -> False
        -- built-ins should not be used as type constructors
        it "type Something = String in True" $ do
          result <- eval stdLib "type Something = String in True"
          result `shouldSatisfy` isLeft
        it "type Pair a b = Pair (a,b)" $ do
          result <- eval stdLib "type Pair a b = Pair (a,b) in True"
          result `shouldSatisfy` isRight
        it "type Record a = Record { name: String, other: a } in True" $ do
          result <- eval stdLib "type Record a = Record { name: String, other: a } in True"
          result `shouldSatisfy` isRight
        it "type State s a = State (s -> (a,s)) in True" $ do
          result <- eval stdLib "type State s a = State (s -> (a,s)) in True"
          result `shouldSatisfy` isRight
