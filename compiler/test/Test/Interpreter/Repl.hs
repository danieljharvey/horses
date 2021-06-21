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
  IO (Either Text (Type (), Expr Name ()))
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
                MyLambda mempty "i" (MyVar mempty "i")
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
        it "type Maybe a = Just a | Nothing in match Just 1 with (Just a) -> eq(100)(a) | Nothing -> False" $ do
          result <- eval stdLib "type Maybe a = Just a | Nothing in match Just 1 with (Just a) -> eq(100)(a) | Nothing -> False"
          result
            `shouldBe` Right
              (MTPrim mempty MTBool, bool False)
        it "type Maybe a = Just a | Nothing in match Just 1 with (Just a) -> True | Nothing -> 1" $ do
          result <- eval stdLib "type Maybe a = Just a | Nothing in match Just 1 with (Just a) -> True | Nothing -> 1"
          result `shouldSatisfy` isLeft
        it "type Maybe a = Just a | Nothing in match Just 1 with (Just a) -> eq(100)(a) | _ -> False" $ do
          result <- eval stdLib "type Maybe a = Just a | Nothing in match Just 1 with (Just a) -> eq(100)(a) | _ -> False"
          result
            `shouldBe` Right
              (MTPrim mempty MTBool, bool False)
        it "type Stuff = Thing String Int in match Thing \"Hello\" 1 with (Thing name num) -> name" $ do
          result <- eval stdLib "type Stuff = Thing String Int in match Thing \"Hello\" 1 with (Thing name num) -> name"
          result
            `shouldBe` Right
              (MTPrim mempty MTString, str' "Hello")
        it "type Result e a = Failure e | Success a in match Failure \"oh no\" with (Success a) -> \"oh yes\" | (Failure e) -> e" $ do
          result <- eval stdLib "type Result e a = Failure e | Success a in match Failure \"oh no\" with (Success a) -> \"oh yes\" | (Failure e) -> e"
          result
            `shouldBe` Right
              (MTPrim mempty MTString, str' "oh no")
        it "type Blap a = Boop a Int in match Boop True 100 with (Boop a b) -> a" $ do
          result <- eval stdLib "type Blap a = Boop a Int in match Boop True 100 with (Boop a b) -> a"
          result `shouldBe` Right (MTPrim mempty MTBool, bool True)
        it "type Maybe a = Just a | Nothing in match Nothing with Nothing False" $ do
          result <- eval stdLib "type Maybe a = Just a | Nothing in match Nothing with Nothing False"
          result `shouldSatisfy` isLeft
        it "type Thing = Thing String in let a = Thing \"string\" in match a with (Thing s) -> s" $ do
          result <- eval stdLib "type Thing = Thing String in let a = Thing \"string\" in match a with (Thing s) -> s"
          result `shouldBe` Right (MTPrim mempty MTString, str' "string")
        it "type Pair a b = Pair a b in match Pair \"dog\" 1 with Pair \a -> a" $ do
          result <- eval stdLib "type Pair a b = Pair a b in match Pair \"dog\" 1 with Pair \a -> a"
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
        it "type Maybe a = Just a | Nothing in match Just True with Just \\a -> a | Nothing \"what\"" $ do
          result <- eval stdLib "type Maybe a = Just a | Nothing in match Just True with Just \\a -> a | Nothing \"what\""
          result `shouldSatisfy` isLeft
        it "type Either e a = Left e | Right a in \\f -> \\g -> \\either -> match either with (Left e) -> g(e) | (Right a) -> f(a)" $ do
          result <- eval stdLib "type Either e a = Left e | Right a in \\f -> \\g -> \\either -> match either with (Left e) -> g(e) | (Right a) -> f(a)"
          result `shouldSatisfy` isRight
        {-
              it "type Maybe a = Just a | Nothing in \\maybe -> match maybe with Just \\a -> a | Nothing \"poo\"" $ do
                result <- eval stdLib "type Maybe a = Just a | Nothing in \\maybe -> match maybe with Just \\a -> a | Nothing \"poo\""
                fst <$> result
                  `shouldBe` Right
                    ( MTFunction (MTData ( "Maybe") []) (MTPrim MTString)
                    )

        -}
        it "type Array a = Empty | Item a (Array a) in match (Item 1 (Item 2 Empty)) with Empty -> Empty | (Item a rest) -> rest" $ do
          result <- eval stdLib "type Array a = Empty | Item a (Array a) in match (Item 1 (Item 2 Empty)) with Empty -> Empty | (Item a rest) -> rest"
          result
            `shouldBe` Right
              ( MTData mempty "Array" [MTPrim mempty MTInt],
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
        it "type Nat = Zero | Suc Nat in let loop = (\\as -> match as with Zero -> 0 | (Suc as2) -> incrementInt(loop(as2))) in loop(Suc Suc Suc Zero)" $ do
          result <- eval stdLib "type Nat = Zero | Suc Nat in let loop = (\\as -> match as with Zero -> 0 | (Suc as2) -> incrementInt(loop(as2))) in loop(Suc Suc Suc Zero)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 3)
        it "type Nat = Zero | Suc Nat in let loop = (\\as -> \\b -> match as with Zero -> b | (Suc as2) -> incrementInt(loop(as2)(b))) in loop(Suc Suc Suc Zero)(10)" $ do
          result <- eval stdLib "type Nat = Zero | Suc Nat in let loop = (\\as -> \\b -> match as with Zero -> b | (Suc as2) -> incrementInt(loop(as2)(b))) in loop(Suc Suc Suc Zero)(10)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 13)
        {-
              it "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA(addInt(b)(a))(rest)) in reduceA(0)(Item 3 Empty)" $ do
                result <- eval stdLib "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA(addInt(b)(a))(rest)) in reduceA(0)(Item 3 Empty)"
                result `shouldBe` Right (MTPrim mempty MTInt, int 3)
        -}
        it "type Array a = Empty | Item a (Array a) in let reduceA = (\\f -> \\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA(f)(f(b)(a))(rest)) in reduceA(addInt)(0)(Empty)" $ do
          result <- eval stdLib "type Array a = Empty | Item a (Array a) in let reduceA = (\\f -> \\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA(f)(f(b)(a))(rest)) in reduceA(addInt)(0)(Empty)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 0)
        it "type Array a = Empty | Item a (Array a) in let reduceA = (\\f -> \\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA(f)(f(b)(a))(rest)) in reduceA(addInt)(0)(Item 3 Empty)" $ do
          result <- eval stdLib "type Array a = Empty | Item a (Array a) in let reduceA = (\\f -> \\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA(f)(f(b)(a))(rest)) in reduceA(addInt)(0)(Item 3 Empty)"
          result `shouldBe` Right (MTPrim mempty MTInt, int 3)
        it "type Tlee a = Non | Tlee (Option b) in {}" $ do
          result <- eval stdLib "type Tlee a = Non | Tlee (Option b) in {}"
          result `shouldSatisfy` isLeft
        it "let some = \\a -> Some a in if True then some(1) else None" $ do
          result <- eval stdLib "let some = \\a -> Some a in if True then some(1) else None"
          result
            `shouldBe` Right
              ( MTData mempty "Option" [MTPrim mempty MTInt],
                MyConsApp
                  mempty
                  (MyConstructor mempty "Some")
                  (int 1)
              )
        it "\\a -> match a with (Some as) -> True | None 100" $ do
          result <- eval stdLib "\\a -> match a with (Some as) -> True | None 100"
          fst <$> result
            `shouldSatisfy` isLeft
        it "\\a -> match a with (Some as) -> as | None -> 100" $ do
          result <- eval stdLib "\\a -> match a with (Some as) -> as | None -> 100"
          fst <$> result
            `shouldBe` Right
              ( MTFunction
                  mempty
                  (MTData mempty "Option" [MTPrim mempty MTInt])
                  (MTPrim mempty MTInt)
              )
        it "fromMaybe should fail typecheck when default does not match inner value" $ do
          result <- eval stdLib "let fromMaybe = \\def -> (\\maybe -> match maybe with (Some a) -> a | None -> def) in fromMaybe(\"Horse\")(Some 1)"
          result `shouldSatisfy` isLeft
        it "fromMaybe works when types match up" $ do
          result <- eval stdLib "let fromMaybe = \\def -> (\\maybe -> match maybe with (Some a) -> a | None -> def) in fromMaybe(\"Horse\")(Some \"Dog\")"
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
        it "\"dog\" ++ \"log\"" $ do
          result <- eval stdLib "\"dog\" ++ \"log\""
          result `shouldBe` Right (MTPrim mempty MTString, str' "doglog")
        it "\"dog\" ++ 123" $ do
          result <- eval stdLib "\"dog\" ++ 123"
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
                  ( MTRecordRow
                      mempty
                      (M.singleton "one" (MTVar mempty (tvFree 1)))
                      (unknown 2)
                  )
                  ( MTFunction
                      mempty
                      ( MTRecordRow
                          mempty
                          ( M.fromList
                              [ ("one", MTVar mempty (tvFree 3)),
                                ( "two",
                                  MTVar mempty (tvFree 4)
                                )
                              ]
                          )
                          (unknown 5)
                      )
                      (MTVar mempty (tvFree 3))
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
                      "r"
                      ( MyInfix
                          mempty
                          Add
                          (MyVar mempty "r")
                          (int 100)
                      )
                  )
              )
        it "\\state -> \\s -> match state with (State sas) -> sas(s)" $ do
          result <- eval stdLib "\\state -> \\s -> match state with (State sas) -> sas(s)"
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
        it "Stops boolean and Option<A> being used together" $ do
          result <- eval stdLib "\\some -> match some with (Some a) -> Some (a == 1) | _ -> some"
          result `shouldSatisfy` isLeft
        -- this should be thrown out by the interpreter
        it "Interpreter is stopped before it loops infinitely" $ do
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
        it "\\person -> match person with (Person p) -> p.age" $ do
          result <- eval stdLib "\\person -> match person with (Person p) -> p.age"
          result `shouldSatisfy` isRight
        -- simplest swaps test
        it "\\a -> 1" $ do
          result <- eval mempty "\\a -> 1"
          case result of
            Left _ -> error "Was not supposed to fail"
            Right (_, expr') -> T.unpack (prettyPrint expr') `shouldContain` "a"

        -- filter function for strings
        it "let filter = \\pred -> \\str -> let fn = (\\s -> match s with a ++ as -> let rest = fn(as); if pred(a) then a ++ rest else rest | _ -> \"\") in fn(str); filter(\\a -> a == \"o\")(\"woo\")" $ do
          result <- eval stdLib "let filter = \\pred -> \\str -> let fn = (\\s -> match s with a ++ as -> let rest = fn(as); if pred(a) then a ++ rest else rest | _ -> \"\") in fn(str); filter(\\a -> a == \"o\")(\"woo\")"
          result `shouldBe` Right (MTPrim mempty MTString, MyLiteral mempty (MyString "oo"))

        it "runParser(anyChar)(\"dog\")" $ do
          result <- eval stdLib "runParser(anyChar)(\"dog\")"
          result `shouldSatisfy` isRight

        xit "let repeat = fmapParser(\\a -> a ++ a)(anyChar) in runParser(repeat)(\"dog\")" $ do
          result <- eval stdLib "let repeat = fmapParser(\\a -> a ++ a)(anyChar) in runParser(repeat)(\"dog\")"
          snd <$> result
            `shouldBe` Right
              ( MyConsApp
                  mempty
                  (MyConstructor mempty "Some")
                  (MyLiteral mempty (MyString "dd"))
              )
        xit "let parser = bindParser(\\a -> if a == \"d\" then anyChar else failParser)(anyChar); runParser(parser)(\"dog\")" $ do
          result <- eval stdLib "let parser = bindParser(\\a -> if a == \"d\" then anyChar else failParser)(anyChar); runParser(parser)(\"dog\")"
          snd <$> result
            `shouldBe` Right
              ( MyConsApp
                  mempty
                  (MyConstructor mempty "Some")
                  (MyLiteral mempty (MyString "o"))
              )
        xit "let parser = bindParser(\\a -> if a == \"d\" then anyChar else failParser)(anyChar); runParser(parser)(\"log\")" $ do
          result <- eval stdLib "let parser = bindParser(\\a -> if a == \"d\" then anyChar else failParser)(anyChar); runParser(parser)(\"log\")"
          snd <$> result
            `shouldBe` Right
              (MyConstructor mempty "None")
        it "[1,2,3]" $ do
          result <- eval stdLib "[1,2,3]"
          result
            `shouldBe` Right
              ( MTArray mempty (MTPrim mempty MTInt),
                MyArray mempty [int 1, int 2, int 3]
              )
        it "[1,True,3]" $ do
          result <- eval stdLib "[1,True,3]"
          result
            `shouldSatisfy` isLeft

        describe "Native array" $ do
          it "[1] <> [2]" $ do
            result <- eval stdLib "[1] <> [2]"
            result
              `shouldBe` Right
                ( MTArray mempty (MTPrim mempty MTInt),
                  MyArray mempty [int 1, int 2]
                )
          it "[1] <> [True]" $ do
            result <- eval stdLib "[1] <> [True]"
            result `shouldSatisfy` isLeft
          it "[1] <> \"2\"" $ do
            result <- eval stdLib "[1] <> \"2\""
            result
              `shouldSatisfy` isLeft
          it "\"1\" <> [2]" $ do
            result <- eval stdLib "\"1\" <> [2]"
            result
              `shouldSatisfy` isLeft
          it "mapArray(\\a -> a + 1)([1,2,3])" $ do
            result <- eval stdLib "mapArray(\\a -> a + 1)([1,2,3])"
            result
              `shouldBe` Right
                ( MTArray mempty (MTPrim mempty MTInt),
                  MyArray mempty [int 2, int 3, int 4]
                )
        describe "Pattern matching" $ do
          it "Matches a wildcard" $ do
            result <- eval stdLib "match 1 with _ -> True"
            result `shouldBe` Right (MTPrim mempty MTBool, bool True)
          it "Matches a variable" $ do
            result <- eval stdLib "match 1 with a -> a"
            result `shouldBe` Right (MTPrim mempty MTInt, int 1)
          it "Deconstructs a pair" $ do
            result <- eval stdLib "match (1,True) with (a,b) -> b"
            result `shouldBe` Right (MTPrim mempty MTBool, bool True)
          it "Matches an int literal" $ do
            result <- eval stdLib "match (1, True) with (1, a) -> a | _ -> False"
            result `shouldBe` Right (MTPrim mempty MTBool, bool True)
          it "Matches a string literal" $ do
            result <- eval stdLib "match \"dog\" with \"dog\" -> True | _ -> False"
            result `shouldBe` Right (MTPrim mempty MTBool, bool True)
          it "Matches two string literals" $ do
            result <- eval stdLib "match \"dog\" with \"dog\" -> True | \"log\" -> True | _ -> False"
            result `shouldBe` Right (MTPrim mempty MTBool, bool True)
          it "Matches a record" $ do
            result <- eval stdLib "match { dog: 1 } with { dog: a } -> a"
            result `shouldBe` Right (MTPrim mempty MTInt, int 1)
          it "Matches a constructor with no args" $ do
            result <- eval stdLib "match None with None -> False | _ -> True"
            result `shouldBe` Right (MTPrim mempty MTBool, bool False)
          it "Matches a constructor with args" $ do
            result <- eval stdLib "match Some 1 with (Some _) -> True | None -> False"
            result `shouldBe` Right (MTPrim mempty MTBool, bool True)
          it "Matches These correctly" $ do
            result <- eval stdLib "match This 1 with (These _ _) -> True | _ -> False"
            result `shouldBe` Right (MTPrim mempty MTBool, bool False)
          it "Typechecks Either correctly" $ do
            result <- eval stdLib "match Right 100 with (Left \"log\") -> False | (Right 100) -> True | _ -> False"
            result `shouldBe` Right (MTPrim mempty MTBool, bool True)
          it "Does not have a swap error" $ do
            result <- eval stdLib "\\a -> match (Left a) with (Left e) -> e | _ -> False"
            result `shouldSatisfy` isRight
          it "Pulls Left into scope from Project" $ do
            result <- eval stdLib "\\a -> match a with (Left e) -> e | _ -> False"
            result `shouldSatisfy` isRight
          it "Parses constructor application in expr" $ do
            result <- eval stdLib "match Some 1 with (Some a) -> Some a | _ -> None"
            result `shouldSatisfy` isRight
          it "Parses and pretty prints more complex matches" $ do
            result <- eval stdLib "\\mf -> \\ma -> match (mf, ma) with (Right f, Right a) -> Right f(a) | (Left e, _) -> Left e | (_, Left e) -> Left e"
            result `shouldSatisfy` isRight
          it "Matches array with non-empty match" $ do
            result <- eval stdLib "match [1] with [_] -> True | _ -> False"
            result `shouldBe` Right (MTPrim mempty MTBool, bool True)
          it "Matches empty array with empty case" $ do
            result <- eval stdLib "match [] with [_] -> True | _ -> False"
            result `shouldBe` Right (MTPrim mempty MTBool, bool False)
          it "Should not match when input array is longer than pattern" $ do
            result <- eval stdLib "match [1,2] with [_] -> True | _ -> False"
            result `shouldBe` Right (MTPrim mempty MTBool, bool False)
          it "Should match when input is long but we have a SpreadWildcard at the end" $ do
            result <- eval stdLib "match [1,2,3] with [1,2,...] -> True | _ -> False"
            result `shouldBe` Right (MTPrim mempty MTBool, bool True)
          it "Shouldn't match when input is too short with SpreadWildcard at the end" $ do
            result <- eval stdLib "match [1] with [1,2,...] -> True | _ -> False"
            result `shouldBe` Right (MTPrim mempty MTBool, bool False)
          it "Binds empty remainder with array to SpreadValue" $ do
            result <- eval stdLib "match [1] with [1,...a] -> a | _ -> [0]"
            result
              `shouldBe` Right
                ( MTArray mempty (MTPrim mempty MTInt),
                  MyArray mempty mempty
                )
          it "Binds remainder with array to SpreadValue" $ do
            result <- eval stdLib "match [1,2,3] with [1,...a] -> a | _ -> []"
            result
              `shouldBe` Right
                ( MTArray mempty (MTPrim mempty MTInt),
                  MyArray mempty [int 2, int 3]
                )
          it "Errors if we bind the same variable twice" $ do
            result <- eval stdLib "match (1,2) with (a,a) -> a"
            result `shouldSatisfy` isLeft
          it "Uses a constructor inside an array" $ do
            result <- eval stdLib "match [] with [Some 1] -> True | _ -> False"
            result `shouldBe` Right (MTPrim mempty MTBool, bool False)
          it "Generates more nuanced exhaustiveness checks when using spread operatpr" $ do
            result <- eval stdLib "match [] with [] -> True | [_] -> False | [_,...] -> False"
            result
              `shouldBe` Right
                (MTPrim mempty MTBool, bool True)
          it "Matches an empty string" $ do
            result <- eval stdLib "match \"\" with _ ++ _ -> True | \"\" -> False"
            result `shouldBe` Right (MTPrim mempty MTBool, bool False)
          it "Matches an non-empty string" $ do
            result <- eval stdLib "match \"dog\" with a ++ b -> (a,b) | \"\" -> (\"\", \"\")"
            result
              `shouldBe` Right
                ( MTPair mempty (MTPrim mempty MTString) (MTPrim mempty MTString),
                  MyPair mempty (MyLiteral mempty (MyString "d")) (MyLiteral mempty (MyString "og"))
                )
          it "Fix empty pattern match obscuring bindings" $ do
            result <- eval stdLib "\\a -> match None with (None) -> a | _ -> a"
            result `shouldSatisfy` isRight
