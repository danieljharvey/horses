{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Modules.Repl
  ( spec,
  )
where

-- testing doing repl things but in the modules world

import Data.Either (isLeft, isRight)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.ExprUtils
import Language.Mimsa.Printer
import Language.Mimsa.Project.Stdlib (buildStdlib)
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.NormaliseTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

stdlib :: Project Annotation
stdlib = fromRight buildStdlib

eval ::
  Text ->
  IO (Either Text (Type (), Expr Name ()))
eval input = do
  let action = do
        expr <- Actions.parseExpr input
        (mt, interpretedExpr, _) <-
          Actions.evaluateModule expr mempty
        pure (mt, interpretedExpr)
  case Actions.run stdlib action of
    Right (_, _, (mt, endExpr)) -> do
      pure (Right (normaliseType (toEmptyType mt), toEmptyAnn endExpr))
    Left e -> pure (Left (prettyPrint e))

-- remove annotations for comparison
toEmptyAnn :: Expr a b -> Expr a ()
toEmptyAnn = toEmptyAnnotation

toEmptyType :: Type a -> Type ()
toEmptyType a = a $> ()

spec :: Spec
spec =
  describe "Modules repl" $ do
    describe "End to end parsing to evaluation" $ do
      it "Use Prelude.fst" $ do
        result <- eval "let x = ((1,2)) in Prelude.fst x"
        result
          `shouldBe` Right
            (MTPrim mempty MTInt, int 1)

      it "Access value inside record" $ do
        result <- eval "let good = ({ dog: True }) in good.dog"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Run function from record" $ do
        result <- eval "let record = ({ id: (\\i -> i) }) in record.id"
        result
          `shouldBe` Right
            ( MTFunction mempty (unknown 1) (unknown 1),
              MyLambda mempty (Identifier mempty "i") (MyVar mempty Nothing "i")
            )

      it "Use polymorphic function from inside record" $ do
        result <- eval "let prelude = ({ id: (\\i -> i) }) in prelude.id 1"
        result
          `shouldBe` Right
            ( MTPrim mempty MTInt,
              int 1
            )

      it "Calls function from doubly nested record" $ do
        result <- eval "let bigPrelude = ({ prelude: { id: (\\i -> i) } }) in bigPrelude.prelude.id 1"
        result
          `shouldBe` Right
            ( MTPrim mempty MTInt,
              int 1
            )

      it "compose incrementInt" $ do
        result <- eval "let incrementInt a = a + 1; let compose = (\\f -> \\g -> \\a -> f (g a)) in let blah = compose incrementInt incrementInt in blah 67"
        result `shouldBe` Right (MTPrim mempty MTInt, int 69)

      it "Use id function twice" $ do
        result <- eval "let reuse = ({ first: Prelude.id 1, second: Prelude.id 2 }) in reuse.first"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)

      it "Define and use id function" $ do
        result <- eval "let id = \\a -> a in id 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)

      it "Use id function twice with different types" $ do
        result <- eval "let reuse = ({ first: Prelude.id True, second: Prelude.id 2 }) in reuse.first"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Use id function with different types (2)" $ do
        result <- eval "let reuse = ({ first: Prelude.id, second: Prelude.id 2 }) in reuse.first True"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "reuses polymorphic function" $ do
        result <- eval "let reuse = ({ first: Prelude.const 1, second: Prelude.const True }) in reuse.first 100"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)

      it "reuses polymorphic function 2" $ do
        result <- eval "let reuse = ({ first: Prelude.const True, second: Prelude.const 2 }) in reuse.second 100"
        result `shouldBe` Right (MTPrim mempty MTInt, int 2)

      it "reuses polymorphic function defined here" $ do
        result <- eval "let id2 a = a; (id2 1, id2 True)"
        result `shouldSatisfy` isRight

      it "Use a function" $ do
        result <- eval "let addInt a b = a + b; addInt 1 2"
        result `shouldBe` Right (MTPrim mempty MTInt, int 3)

      it "(\\a -> a) 1" $ do
        result <- eval "(\\a -> a) 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)

      it "(\\b -> (\\a -> b)) 0 1" $ do
        result <- eval "(\\b -> (\\a -> b)) 0 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 0)

      it "Nested function application" $ do
        result <- eval "let addInt a b = a + b; addInt 1 (addInt (addInt 2 4) 5)"
        result `shouldBe` Right (MTPrim mempty MTInt, int 12)

      it "type LeBool = Vrai | Faux in Vrai" $ do
        result <- eval "type LeBool = Vrai | Faux in Vrai"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty Nothing "LeBool" [],
              MyConstructor mempty Nothing "Vrai"
            )

      it "type Nat = Zero | Suc Nat in Suc Zero" $ do
        result <- eval "type Nat = Zero | Suc Nat in Suc Zero"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty Nothing "Nat" [],
              MyApp
                mempty
                (MyConstructor mempty Nothing "Suc")
                (MyConstructor mempty Nothing "Zero")
            )

      it "type Nat = Zero | Suc Nat in Suc (Suc Zero)" $ do
        result <- eval "type Nat = Zero | Suc Nat in Suc (Suc Zero)"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty Nothing "Nat" [],
              MyApp
                mempty
                (MyConstructor mempty Nothing "Suc")
                ( MyApp
                    mempty
                    (MyConstructor mempty Nothing "Suc")
                    (MyConstructor mempty Nothing "Zero")
                )
            )

      it "type Nat = Zero | Suc Nat in Suc 1" $ do
        result <- eval "type Nat = Zero | Suc Nat in Suc 1"
        result
          `shouldSatisfy` isLeft

      it "type Nat = Zero | Suc Nat in Suc Dog" $ do
        result <- eval "type Nat = Zero | Suc Nat in Suc Dog"
        result
          `shouldSatisfy` isLeft

      it "type Nat = Zero | Suc Nat in Suc" $ do
        result <- eval "type Nat = Zero | Suc Nat in Suc"
        result
          `shouldBe` Right
            ( MTFunction
                mempty
                (dataTypeWithVars mempty Nothing "Nat" [])
                (dataTypeWithVars mempty Nothing "Nat" []),
              MyConstructor mempty Nothing "Suc"
            )

      it "type OhNat = Zero | Suc OhNat String in Suc" $ do
        result <- eval "type OhNat = Zero | Suc OhNat String in Suc"
        result
          `shouldBe` Right
            ( MTFunction
                mempty
                (dataTypeWithVars mempty Nothing "OhNat" [])
                ( MTFunction
                    mempty
                    (MTPrim mempty MTString)
                    (dataTypeWithVars mempty Nothing "OhNat" [])
                ),
              MyConstructor mempty Nothing "Suc"
            )
      it "type Pet = Cat String | Dog String in Cat \"mimsa\"" $ do
        result <- eval "type Pet = Cat String | Dog String in Cat \"mimsa\""
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty Nothing "Pet" [],
              MyApp
                mempty
                (MyConstructor mempty Nothing "Cat")
                (str' "mimsa")
            )
      it "type Void in 1" $ do
        result <- eval "type Void in 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)
      it "type String = Should | Error in Error" $ do
        result <- eval "type String = Should | Error in Error"
        result `shouldSatisfy` isLeft
      it "type LongBoy = Stuff String Int String in Stuff \"yes\"" $ do
        result <- eval "type LongBoy = Stuff String Int String in Stuff \"yes\""
        result
          `shouldBe` Right
            ( MTFunction
                mempty
                (MTPrim mempty MTInt)
                ( MTFunction
                    mempty
                    (MTPrim mempty MTString)
                    (dataTypeWithVars mempty Nothing "LongBoy" [])
                ),
              MyApp
                mempty
                (MyConstructor mempty Nothing "Stuff")
                (str' "yes")
            )
      it "type Tree = Leaf Int | Branch Tree Tree in Branch (Leaf 1) (Leaf 2)" $ do
        result <- eval "type Tree = Leaf Int | Branch Tree Tree in Branch (Leaf 1) (Leaf 2)"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty Nothing "Tree" [],
              MyApp
                mempty
                ( MyApp
                    mempty
                    (MyConstructor mempty Nothing "Branch")
                    (MyApp mempty (MyConstructor mempty Nothing "Leaf") (int 1))
                )
                (MyApp mempty (MyConstructor mempty Nothing "Leaf") (int 2))
            )
      it "type Maybe a = Just a | Nothing in Just" $ do
        result <- eval "type Maybe a = Just a | Nothing in Just"
        result
          `shouldBe` Right
            ( MTFunction
                mempty
                (MTVar mempty (TVUnificationVar 1))
                (dataTypeWithVars mempty Nothing "Maybe" [MTVar mempty (TVUnificationVar 1)]),
              MyConstructor mempty Nothing "Just"
            )
      it "type Maybe a = Just a | Nothing in Nothing" $ do
        result <- eval "type Maybe a = Just a | Nothing in Nothing"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty Nothing "Maybe" [MTVar mempty (TVUnificationVar 1)],
              MyConstructor mempty Nothing "Nothing"
            )
      it "type Maybe a = Just a | Nothing in Just 1" $ do
        result <- eval "type Maybe a = Just a | Nothing in Just 1"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty Nothing "Maybe" [MTPrim mempty MTInt],
              MyApp
                mempty
                (MyConstructor mempty Nothing "Just")
                (int 1)
            )

      it "use Maybe with eq" $ do
        result <- eval "let eq a b = a == b; type Maybe a = Just a | Nothing in match Just 1 with (Just a) -> eq 100 a | Nothing -> False"
        result
          `shouldBe` Right
            (MTPrim mempty MTBool, bool False)

      it "type Maybe a = Just a | Nothing in match Just 1 with (Just a) -> True | Nothing -> 1" $ do
        result <- eval "type Maybe a = Just a | Nothing in match Just 1 with (Just a) -> True | Nothing -> 1"
        result `shouldSatisfy` isLeft

      it "unfolding Maybe more" $ do
        result <- eval "let eq a b = a == b; type Maybe a = Just a | Nothing in match Just 1 with (Just a) -> eq 100 a | _ -> False"
        result
          `shouldBe` Right
            (MTPrim mempty MTBool, bool False)

      it "Extracts values with pattern match" $ do
        result <- eval "type Stuff = Thing String Int in match Thing \"Hello\" 1 with (Thing name num) -> name"
        result
          `shouldBe` Right
            (MTPrim mempty MTString, str' "Hello")

      it "Pattern matching on result" $ do
        result <- eval "type Result e a = Failure e | Success a in match Failure \"oh no\" with (Success a) -> \"oh yes\" | (Failure e) -> e"
        result
          `shouldBe` Right
            (MTPrim mempty MTString, str' "oh no")

      it "Pattern matching datatype with type vars and concrete types" $ do
        result <- eval "type Blap a = Boop a Int in match Boop True 100 with (Boop a b) -> a"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Identifies broken pattern match" $ do
        result <- eval "type Maybe a = Just a | Nothing in match Nothing with Nothing False"
        result `shouldSatisfy` isLeft

      it "type Thing = Thing String in let a = Thing \"string\" in match a with (Thing s) -> s" $ do
        result <- eval "type Thing = Thing String in let a = Thing \"string\" in match a with (Thing s) -> s"
        result `shouldBe` Right (MTPrim mempty MTString, str' "string")

      it "type Pair a b = Pair a b in match Pair \"dog\" 1 with Pair \a -> a" $ do
        result <- eval "type Pair a b = Pair a b in match Pair \"dog\" 1 with Pair \a -> a"
        result `shouldSatisfy` isLeft

      it "type Tree a = Leaf a | Branch (Tree a) (Tree a) in Leaf 1" $ do
        result <- eval "type Tree a = Leaf a | Branch (Tree a) (Tree a) in Leaf 1"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty Nothing "Tree" [MTPrim mempty MTInt],
              MyApp
                mempty
                (MyConstructor mempty Nothing "Leaf")
                (int 1)
            )
      it "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Leaf 1" $ do
        result <- eval "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Leaf 1"
        result
          `shouldSatisfy` isLeft

      it "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Branch (Leaf 1) (Leaf True)" $ do
        result <- eval "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Branch (Leaf 1) (Leaf True)"
        result
          `shouldSatisfy` isLeft

      it "type Tree a = Empty | Branch (Tree a) a (Tree a) in Branch (Empty) 1 (Empty)" $ do
        result <- eval "type Tree a = Empty | Branch (Tree a) a (Tree a) in Branch (Empty) 1 (Empty)"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty Nothing "Tree" [MTPrim mempty MTInt],
              MyApp
                mempty
                ( MyApp
                    mempty
                    ( MyApp
                        mempty
                        (MyConstructor mempty Nothing "Branch")
                        (MyConstructor mempty Nothing "Empty")
                    )
                    (int 1)
                )
                (MyConstructor mempty Nothing "Empty")
            )

      it "unwrapping Maybe" $ do
        result <- eval "type Maybe a = Just a | Nothing in match Just True with Just \\a -> a | Nothing \"what\""
        result `shouldSatisfy` isLeft

      it "unwrap for Either" $ do
        result <- eval "type Either e a = Left e | Right a in \\f -> \\g -> \\either -> match either with (Left e) -> g e | (Right a) -> (f a)"
        result `shouldSatisfy` isRight
      {-
            it "type Maybe a = Just a | Nothing in \\maybe -> match maybe with Just \\a -> a | Nothing \"poo\"" $ do
              result <- eval "type Maybe a = Just a | Nothing in \\maybe -> match maybe with Just \\a -> a | Nothing \"poo\""
              fst <$> result
                `shouldBe` Right
                  ( MTFunction (dataTypeWithVars ( "Maybe") []) (MTPrim MTString)
                  )

      -}
      it "type Array a = Empty | Item a (Array a) in match (Item 1 (Item 2 Empty)) with Empty -> Empty | (Item a rest) -> rest" $ do
        result <- eval "type Array a = Empty | Item a (Array a) in match (Item 1 (Item 2 Empty)) with Empty -> Empty | (Item a rest) -> rest"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty Nothing "Array" [MTPrim mempty MTInt],
              MyApp
                mempty
                ( MyApp
                    mempty
                    ( MyConstructor
                        mempty
                        Nothing
                        "Item"
                    )
                    (int 2)
                )
                (MyConstructor mempty Nothing "Empty")
            )

      it "Recursive function works" $ do
        result <- eval "let eq a b = a == b; let addInt a b = a + b; let loop = (\\a -> if eq 10 a then a else loop (addInt a 1)) in loop 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 10)

      it "Recursively converts Nat to integer" $ do
        result <- eval "let incrementInt a = a + 1; type Nat = Zero | Suc Nat in let loop = (\\as -> match as with Zero -> 0 | (Suc as2) -> incrementInt (loop as2)) in loop (Suc (Suc (Suc Zero)))"
        result `shouldBe` Right (MTPrim mempty MTInt, int 3)

      it "Recursively converts bigger Nat to integer" $ do
        result <- eval "let incrementInt a = a + 1; type Nat = Zero | Suc Nat in let loop = (\\as -> \\b -> match as with Zero -> b | (Suc as2) -> incrementInt (loop as2 b)) in loop (Suc (Suc (Suc Zero))) 10"
        result `shouldBe` Right (MTPrim mempty MTInt, int 13)
      {-
            it "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA(addInt(b)(a))(rest)) in reduceA(0)(Item 3 Empty)" $ do
              result <- eval "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA(addInt(b)(a))(rest)) in reduceA(0)(Item 3 Empty)"
              result `shouldBe` Right (MTPrim mempty MTInt, int 3)
      -}

      it "Array reduce function" $ do
        result <- eval "let addInt a b = a + b; type Array a = Empty | Item a (Array a) in let reduceA = (\\f -> \\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA f (f b a) rest) in reduceA addInt 0 Empty"
        result `shouldBe` Right (MTPrim mempty MTInt, int 0)

      it "Array reduce function 2" $ do
        result <- eval "let addInt a b = a + b; type Array a = Empty | Item a (Array a) in let reduceA = (\\f -> \\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA f (f b a) rest) in reduceA addInt 0 (Item 3 Empty)"
        result `shouldBe` Right (MTPrim mempty MTInt, int 3)

      it "type Tlee a = Non | Tlee (Maybe b) in {}" $ do
        result <- eval "type Tlee a = Non | Tlee (Maybe b) in {}"
        result `shouldSatisfy` isLeft

      it "Use Maybe module" $ do
        result <- eval "let some = \\a -> Maybe.Just a in if True then some 1 else Maybe.Nothing"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty (Just "Maybe") "Maybe" [MTPrim mempty MTInt],
              MyApp
                mempty
                (MyConstructor mempty (Just "Maybe") "Just")
                (int 1)
            )

      it "Pattern match fails using Maybe" $ do
        result <- eval "\\a -> match a with (Maybe.Just as) -> True | Maybe.Nothing -> 100"
        fst <$> result
          `shouldSatisfy` isLeft

      it "Pattern match succeeds using Maybe" $ do
        result <- eval "\\a -> match a with (Maybe.Just as) -> as | Maybe.Nothing -> 100"
        fst <$> result
          `shouldBe` Right
            ( MTFunction
                mempty
                (dataTypeWithVars mempty (Just "Maybe") "Maybe" [MTPrim mempty MTInt])
                (MTPrim mempty MTInt)
            )

      it "fromMaybe should fail typecheck when default does not match inner value" $ do
        result <- eval "let fromMaybe = \\defVal -> (\\maybe -> match maybe with (Maybe.Just a) -> a | Maybe.Nothing -> defVal) in fromMaybe \"Horse\" (Maybe.Just 1)"
        result `shouldSatisfy` isLeft

      it "fromMaybe works when types match up" $ do
        result <- eval "let fromMaybe = \\defVal -> (\\maybe -> match maybe with (Maybe.Just a) -> a | Maybe.Nothing -> defVal) in fromMaybe \"Horse\" (Maybe.Just \"Dog\")"
        result `shouldBe` Right (MTPrim mempty MTString, str' "Dog")

      it "String and bool fail to equals" $ do
        result <- eval "True == \"dog\""
        result `shouldSatisfy` isLeft

      it "Errors when attempting function equality" $ do
        -- no function equality
        result <- eval "(\\a -> a) == (\\b -> b)"
        result `shouldSatisfy` isLeft

      it "True == False" $ do
        result <- eval "True == False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "True == True" $ do
        result <- eval "True == True"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Equality with constructors" $ do
        result <- eval "(Maybe.Just 1) == Maybe.Just 2"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "Equality as a function" $ do
        result <- eval "let eq1 = (\\a -> a == 1) in eq1 1"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "1 + 1" $ do
        result <- eval "1 + 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 2)

      it "True + 1" $ do
        result <- eval "True + 1"
        result `shouldSatisfy` isLeft

      it "10 - 1" $ do
        result <- eval "10 - 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 9)

      it "True - 1" $ do
        result <- eval "True - 1"
        result `shouldSatisfy` isLeft

      it "1 + 1 + 1 + 1" $ do
        result <- eval "1 + 1 + 1 + 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 4)

      it "\"dog\" ++ \"log\"" $ do
        result <- eval "\"dog\" ++ \"log\""
        result `shouldBe` Right (MTPrim mempty MTString, str' "doglog")

      it "\"dog\" ++ 123" $ do
        result <- eval "\"dog\" ++ 123"
        result `shouldSatisfy` isLeft

      it "passes record to function" $ do
        result <- eval "let f = (\\a -> if True then a.num else a.num2) in f {num: 1, num2: 2}"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)

      it "if True then { one: 1 } else { two: 2 }" $ do
        result <- eval "if True then { one: 1 } else { two: 2 }"
        result `shouldSatisfy` isLeft

      it "if True then { one: 1 } else { one: 2 }" $ do
        result <- eval "if True then { one: 1 } else { one: 2 }"
        result `shouldSatisfy` isRight

      it "let a = { one: 1 }; let one = a.one; let two = a.two; a" $ do
        result <- eval "let a = { one: 1 }; let one = a.one; let two = a.two; a"
        result `shouldSatisfy` isLeft

      it "\\a -> let one = a.one; let two = a.two; a" $ do
        result <- eval "\\a -> let one = a.one; let two = a.two; a"
        result
          `shouldSatisfy` isRight

      it "passes inferred record value to a function" $ do
        result <- eval "let useRecord = (\\a -> let one = a.one; let two = a.two; one + two) in useRecord {one: 1}"
        result `shouldSatisfy` \case
          (Left err) -> not $ T.isInfixOf "InterpreterError" err
          _ -> False

      it "passes inferred value to another function" $ do
        result <- eval "let useRecord = (\\a -> let one = a.one; let two = a.two; one + two) in useRecord {two: 2}"
        result `shouldSatisfy` \case
          (Left err) -> not $ T.isInfixOf "InterpreterError" err
          _ -> False

      it "passes complete record value to function" $ do
        result <- eval "let useRecord = (\\a -> let one = a.one; let two = a.two; one + two) in useRecord {one: 1, two: 2}"
        result `shouldSatisfy` isRight

      it "if ?missingFn then 1 else 2" $ do
        result <- eval "if ?missingFn then 1 else 2"
        result `shouldSatisfy` \case
          (Left msg) ->
            T.isInfixOf "Typed holes found" msg
              && T.isInfixOf "?missingFn" msg
              && T.isInfixOf "Boolean" msg
          (Right _) -> False

      it "typed holes found in map function" $ do
        result <- eval "let map = \\f -> \\a -> f a in map ?flappy 1"
        result `shouldSatisfy` \case
          (Left msg) ->
            T.isInfixOf "Typed holes found" msg
          (Right _) -> False

      it "compose function" $ do
        result <- eval "let compose = \\f -> \\g -> \\a -> f (g a); compose"
        result `shouldSatisfy` isRight

      it "Just (1 == 1)" $ do
        result <- eval "Maybe.Just (1 == 1)"
        snd <$> result
          `shouldBe` Right
            ( MyApp
                mempty
                (MyConstructor mempty (Just "Maybe") "Just")
                (bool True)
            )
      it "\\a -> if (100 == a.int) then 100 else 0" $ do
        result <- eval "\\a -> if (100 == a.int) then 100 else 0"
        result `shouldSatisfy` isRight

      it "\\a -> if (a.one == a.two) then 100 else 0" $ do
        result <- eval "\\a -> if (a.one == a.two) then 100 else 0"
        result `shouldSatisfy` isRight

      it "type Reader r a = Reader (r -> a) in Reader (\\r -> r + 100)" $ do
        result <- eval "type Reader r a = Reader (r -> a) in Reader (\\r -> r + 100)"
        result
          `shouldBe` Right
            ( dataTypeWithVars
                mempty
                Nothing
                "Reader"
                [MTPrim mempty MTInt, MTPrim mempty MTInt],
              MyApp
                mempty
                (MyConstructor mempty Nothing "Reader")
                ( MyLambda
                    mempty
                    (Identifier mempty "r")
                    ( MyInfix
                        mempty
                        Add
                        (MyVar mempty Nothing "r")
                        (int 100)
                    )
                )
            )

      it "Match State monad" $ do
        result <- eval "\\state -> \\s -> match state with (State.State sas) -> sas s"
        result `shouldSatisfy` isRight

      it "Bind with State monad" $ do
        result <- eval "let storeName a = State.fmap (Prelude.const a) (State.put a); let a = State.pure \"dog\"; let b = State.bind storeName a; State.run b \"\""
        result `shouldSatisfy` isRight

      it "Stops boolean and Maybe<A> being used together" $ do
        result <- eval "\\some -> match some with (Maybe.Just a) -> Maybe.Just (a == 1) | _ -> some"
        result `shouldSatisfy` isLeft

      -- need a new way of stopping this looping
      -- perhaps static analysis for silly cases
      -- and a timeout when used as an endpoint?
      xit "Interpreter is stopped before it loops infinitely" $ do
        result <- eval "let forever = \\a -> forever a in forever True"
        result `shouldSatisfy` \case
          Left msg -> "interpreter aborted" `T.isInfixOf` msg
          _ -> False

      -- built-ins should not be used as type constructors
      it "type Justthing = String in True" $ do
        result <- eval "type Justthing = String in True"
        result `shouldSatisfy` isLeft

      it "Define Pair" $ do
        result <- eval "type Pair a b = Pair (a,b) in True"
        result `shouldSatisfy` isRight

      it "type Record a = Record { name: String, other: a } in True" $ do
        result <- eval "type Record a = Record { name: String, other: a } in True"
        result `shouldSatisfy` isRight

      it "type State s a = State (s -> (a,s)) in True" $ do
        result <- eval "type State s a = State (s -> (a,s)) in True"
        result `shouldSatisfy` isRight

      -- simplest swaps test
      it "\\a -> 1" $ do
        result <- eval "\\a -> 1"
        case result of
          Left _ -> error "Was not supposed to fail"
          Right (_, expr') -> T.unpack (prettyPrint expr') `shouldContain` "a"

      it "filter function for strings" $ do
        result <- eval "let filter = \\pred -> \\str -> let fn = (\\s -> match s with a ++ as -> let rest = fn as; if pred a then a ++ rest else rest | _ -> \"\") in fn str; filter (\\a -> a == \"o\") \"woo\""
        result
          `shouldBe` Right
            ( MTPrim mempty MTString,
              MyLiteral mempty (MyString "oo")
            )

      it "Parse any character" $ do
        result <- eval "Parser.run Parser.anyChar \"dog\""
        result `shouldSatisfy` isRight

      it "Parser.fmap works correctly" $ do
        result <- eval "let repeat = Parser.fmap (\\a -> a ++ a) Parser.anyChar in Parser.run repeat \"dog\""
        snd <$> result
          `shouldBe` Right
            ( MyApp
                mempty
                (MyConstructor mempty (Just "Maybe") "Just")
                (MyLiteral mempty (MyString "dd"))
            )

      it "Parser.bind works correctly" $ do
        result <- eval "let parser = Parser.bind (\\a -> if a == \"d\" then Parser.anyChar else Parser.fail) Parser.anyChar; Parser.run parser \"dog\""
        snd <$> result
          `shouldBe` Right
            ( MyApp
                mempty
                (MyConstructor mempty (Just "Maybe") "Just")
                (MyLiteral mempty (MyString "o"))
            )

      it "Parser.bind fails correctly" $ do
        result <- eval "let parser = Parser.bind (\\a -> if a == \"d\" then Parser.anyChar else Parser.fail) Parser.anyChar; Parser.run parser \"log\""
        snd <$> result
          `shouldBe` Right
            (MyConstructor mempty (Just "Maybe") "Nothing")

      it "Parser.ap formats correctly" $ do
        result <- eval "\\parserF -> \\parserA -> let (Parser.Parser pF) = parserF; let (Parser.Parser pA) = parserA; Parser.Parser (\\input -> match (pF input) with Maybe.Just (f, input2) -> (match (pA input2) with Maybe.Just (a, input3) -> Maybe.Just (f a, input3) | _ -> Maybe.Nothing) | _ ->  Maybe.Nothing)"
        result `shouldSatisfy` isRight

      it "Array literal" $ do
        result <- eval "[1,2,3]"
        result
          `shouldBe` Right
            ( MTArray mempty (MTPrim mempty MTInt),
              MyArray mempty [int 1, int 2, int 3]
            )

      it "[1,True,3]" $ do
        result <- eval "[1,True,3]"
        result
          `shouldSatisfy` isLeft

    describe "Native array" $ do
      it "[1] <> [2]" $ do
        result <- eval "[1] <> [2]"
        result
          `shouldBe` Right
            ( MTArray mempty (MTPrim mempty MTInt),
              MyArray mempty [int 1, int 2]
            )

      it "[1] <> [True]" $ do
        result <- eval "[1] <> [True]"
        result `shouldSatisfy` isLeft

      it "[1] <> \"2\"" $ do
        result <- eval "[1] <> \"2\""
        result
          `shouldSatisfy` isLeft

      it "\"1\" <> [2]" $ do
        result <- eval "\"1\" <> [2]"
        result
          `shouldSatisfy` isLeft

      it "Array.fmap increments ints inside" $ do
        result <- eval "Array.fmap (\\a -> a + 1) [1,2,3]"
        result
          `shouldBe` Right
            ( MTArray mempty (MTPrim mempty MTInt),
              MyArray mempty [int 2, int 3, int 4]
            )

    describe "Let pattern" $ do
      it "Matches a wildcard" $ do
        result <- eval "let _ = False in True"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Matches a value" $ do
        result <- eval "let a = True in a"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Matches a pair" $ do
        result <- eval "let (a,b) = (True,False) in a"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Matches a record" $ do
        result <- eval "let { dog: a } = { dog: True } in a"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Does not match a constructor with other cases" $ do
        result <- eval "let (Maybe.Just a) = Maybe.Just True in a"
        result `shouldSatisfy` isLeft

      it "Matches a one case constructor" $ do
        result <- eval "type Ident a = Ident a; let (Ident a) = Ident True in a"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Matches a nested one case constructor" $ do
        result <- eval "type Ident a = Ident a; let (Ident (Ident a)) = Ident (Ident True) in a"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Does not matches a pair that is not complete" $ do
        result <- eval "let (a,True) = (True,False) in a"
        result `shouldSatisfy` isLeft

      it "Adds constructors to required types for StoreExpression" $ do
        result <- eval "let (Parser.Parser parser) = Parser.pred (\\d -> d == \"d\") Parser.anyChar in parser \"dog\""
        result `shouldSatisfy` isRight

    describe "Pattern matching" $ do
      it "Matches a wildcard" $ do
        result <- eval "match 1 with _ -> True"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Matches a variable" $ do
        result <- eval "match 1 with a -> a"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)

      it "Deconstructs a pair" $ do
        result <- eval "match (1,True) with (a,b) -> b"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Matches an int literal" $ do
        result <- eval "match (1, True) with (1, a) -> a | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Matches a string literal" $ do
        result <- eval "match \"dog\" with \"dog\" -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Matches two string literals" $ do
        result <- eval "match \"dog\" with \"dog\" -> True | \"log\" -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Matches a record" $ do
        result <- eval "match { dog: 1 } with { dog: a } -> a"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)

      it "Matches a constructor with no args" $ do
        result <- eval "match Maybe.Nothing with Maybe.Nothing -> False | _ -> True"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "Matches a constructor with args" $ do
        result <- eval "match Maybe.Just 1 with (Maybe.Just _) -> True | Maybe.Nothing -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Matches These correctly" $ do
        result <- eval "match These.This 1 with (These.These _ _) -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "Fallbacks to correct catchall" $ do
        result <- eval "match Maybe.Just 1 with (Maybe.Just _) -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Typechecks Either correctly" $ do
        result <- eval "match Either.Right 100 with (Either.Left \"log\") -> False | (Either.Right 100) -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Does not have a swap error" $ do
        result <- eval "\\a -> match (Either.Left a) with (Either.Left e) -> e | _ -> False"
        result `shouldSatisfy` isRight

      it "Pulls Left into scope from Project" $ do
        result <- eval "\\a -> match a with (Either.Left e) -> e | _ -> False"
        result `shouldSatisfy` isRight

      it "Parses constructor application in expr" $ do
        result <- eval "match Maybe.Just 1 with (Maybe.Just a) -> Maybe.Just a | _ -> Maybe.Nothing"
        result `shouldSatisfy` isRight

      it "Parses and pretty prints more complex matches" $ do
        result <- eval "\\mf -> \\ma -> match (mf, ma) with (Either.Right f, Either.Right a) -> Either.Right (f a) | (Either.Left e, _) -> Either.Left e | (_, Either.Left e) -> Either.Left e"
        result `shouldSatisfy` isRight

      it "Matches array with non-empty match" $ do
        result <- eval "match [1] with [_] -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Matches empty array with empty case" $ do
        result <- eval "match [] with [_] -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "Should not match when input array is longer than pattern" $ do
        result <- eval "match [1,2] with [_] -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "Should match when input is long but we have a SpreadWildcard at the end" $ do
        result <- eval "match [1,2,3] with [1,2,...] -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Shouldn't match when input is too short with SpreadWildcard at the end" $ do
        result <- eval "match [1] with [1,2,...] -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "Binds empty remainder with array to SpreadValue" $ do
        result <- eval "match [1] with [1,...a] -> a | _ -> [0]"
        result
          `shouldBe` Right
            ( MTArray mempty (MTPrim mempty MTInt),
              MyArray mempty mempty
            )
      it "Binds remainder with array to SpreadValue" $ do
        result <- eval "match [1,2,3] with [1,...a] -> a | _ -> []"
        result
          `shouldBe` Right
            ( MTArray mempty (MTPrim mempty MTInt),
              MyArray mempty [int 2, int 3]
            )

      it "Errors if we bind the same variable twice" $ do
        result <- eval "match (1,2) with (a,a) -> a"
        result `shouldSatisfy` isLeft

      it "Uses a constructor inside an array" $ do
        result <- eval "match [] with [Maybe.Just 1] -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "Generates more nuanced exhaustiveness checks when using spread operatpr" $ do
        result <- eval "match [] with [] -> True | [_] -> False | [_,...] -> False"
        result
          `shouldBe` Right
            (MTPrim mempty MTBool, bool True)

      it "Matches an empty string" $ do
        result <- eval "match \"\" with _ ++ _ -> True | \"\" -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "Matches an non-empty string" $ do
        result <- eval "match \"dog\" with a ++ b -> (a,b) | \"\" -> (\"\", \"\")"
        result
          `shouldBe` Right
            ( MTPair mempty (MTPrim mempty MTString) (MTPrim mempty MTString),
              MyPair mempty (MyLiteral mempty (MyString "d")) (MyLiteral mempty (MyString "og"))
            )

      it "Fix empty pattern match obscuring bindings" $ do
        result <- eval "\\a -> match Maybe.Nothing with (Maybe.Nothing) -> a | _ -> a"
        result `shouldSatisfy` isRight

    describe "Error with List type" $ do
      it "Is fine with no shadowed variables" $ do
        let input =
              mconcat
                [ "type List a = Cons a (List a) | Nil; ",
                  "\\a -> \\b -> match (a, b) with ",
                  "(Cons aa restA, Nil) -> (Cons aa restA)",
                  " | (Nil, Cons bb restB) -> (Cons bb restB)",
                  " | _ -> (Nil)"
                ]
        result <- eval input
        result `shouldSatisfy` isRight
      it "Is fine with shadowed variables" $ do
        let input =
              mconcat
                [ "type List a = Cons a (List a) | Nil; ",
                  " \\a -> \\b -> match (a, b) with ",
                  "(Cons a restA, Nil) -> (Cons a restA)",
                  " | (Nil, Cons b restB) -> (Cons b restB)",
                  " | _ -> a"
                ]
        result <- eval input
        result `shouldSatisfy` isRight
    describe "Too many generics in stringReduce" $ do
      it "simpler type" $ do
        let input = "\\a -> match a with head ++ tail -> head | _ -> \"\""
        result <- eval input
        fst <$> result `shouldBe` Right (MTFunction () (MTPrim () MTString) (MTPrim () MTString))
      it "type of function" $ do
        let input = "String.reduce"
        result <- eval input
        fst <$> result
          `shouldBe` Right
            ( MTFunction
                ()
                ( MTFunction
                    ()
                    (MTVar () (TVUnificationVar 1))
                    (MTFunction () (MTPrim () MTString) (MTVar () (TVUnificationVar 1)))
                )
                ( MTFunction
                    ()
                    (MTVar () (TVUnificationVar 1))
                    (MTFunction () (MTPrim () MTString) (MTVar () (TVUnificationVar 1)))
                )
            )

    describe "Monoid losing types" $ do
      it "Monoid.maybe with String.monoid" $ do
        result <- eval "let monoid = Monoid.maybe String.monoid in monoid.mappend (Maybe.Just \"1\") Maybe.Nothing"
        fst <$> result
          `shouldBe` Right
            ( dataTypeWithVars mempty (Just "Maybe") "Maybe" [MTPrim mempty MTString]
            )
      it "Monoid.sum with Monoid.maybe" $ do
        result <- eval "let monoid = Monoid.maybe Monoid.sum; monoid.mappend (Maybe.Just 1) Maybe.Nothing"
        fst <$> result
          `shouldBe` Right
            ( dataTypeWithVars mempty (Just "Maybe") "Maybe" [MTPrim mempty MTInt]
            )

    describe "Tree interpreter error" $ do
      let leaf = MyApp mempty (MyConstructor mempty Nothing "Leaf")
          branch l a =
            MyApp
              mempty
              ( MyApp
                  mempty
                  ( MyApp
                      mempty
                      (MyConstructor mempty Nothing "Branch")
                      l
                  )
                  a
              )
      it "Constructs a Tree" $ do
        result <- eval "Tree.Leaf 1"
        result `shouldSatisfy` isRight

      it "Reverses a leaf" $ do
        result <- eval "Tree.invert (Tree.Leaf 1)"
        snd <$> result
          `shouldBe` Right (leaf (int 1))

      it "Maps a tree" $ do
        result <- eval "Tree.fmap (Prelude.const True) (Tree.Branch (Tree.Leaf 1) 2 (Tree.Leaf 3))"
        result `shouldSatisfy` isRight

      it "Reverses a branch" $ do
        result <- eval "Tree.invert (Tree.Branch (Tree.Leaf 1) 2 (Tree.Leaf 3))"
        snd <$> result
          `shouldBe` Right (branch (leaf (int 3)) (int 2) (leaf (int 1)))

      it "Reverses a small tree correctly" $ do
        result <- eval "Tree.invert (Tree.Branch (Tree.Leaf 1) 2 (Tree.Branch (Tree.Leaf 3) 4 (Tree.Leaf 5)))"
        snd <$> result
          `shouldBe` Right (branch (branch (leaf (int 5)) (int 4) (leaf (int 3))) (int 2) (leaf (int 1)))

      it "Reversing a tree twice is identity" $ do
        result <- eval "let tree = Tree.Branch (Tree.Leaf 1) 2 (Tree.Branch (Tree.Leaf 3) 4 (Tree.Leaf 5)); Tree.invert (Tree.invert tree) == tree"
        snd <$> result
          `shouldBe` Right (bool True)

    describe "delays arity check for infix operators" $ do
      it "is fine" $ do
        result <- eval "let flip f a b = f b a; let and a b = if a then b else False; infix <<>> = flip and; True <<>> False"
        result `shouldSatisfy` isRight

    describe "let with type annotation" $ do
      it "should not parse without brackets" $ do
        result <- eval "let a: Boolean = True in a"
        result `shouldSatisfy` isLeft

      it "should typecheck" $ do
        result <- eval "let (a: Boolean) = True in a"
        result `shouldSatisfy` isRight

      it "should typecheck (with brackets)" $ do
        result <- eval "let (a: Boolean) = True in a"
        result `shouldSatisfy` isRight

      it "should not typecheck" $ do
        result <- eval "let (a: Int) = True in a"
        result `shouldSatisfy` isLeft

      it "should break with non-existent type" $ do
        result <- eval "let (a: FooBar) = True in a"
        result `shouldSatisfy` isLeft

      it "cannot assign concrete value to polymorphic type" $ do
        result <- eval "let (a: anyA) = True in a"
        result `shouldSatisfy` isLeft

    describe "regressions" $ do
      it "should destructure record correctly" $ do
        result <- eval "let { b: b } = { a: 5, b: 100 } in b"
        result `shouldBe` Right (MTPrim mempty MTInt, int 100)
      it "should show pattern match exhaustiveness error" $ do
        result <- eval "let matcher a = match a with [] -> True; matcher [1]"
        result `shouldSatisfy` textErrorContains "Pattern match is not exhaustive"

    describe "lambda with type annotation" $ do
      it "should not parse without brackets" $ do
        result <- eval "\\a -> a + 1 : Int -> Int"
        result `shouldSatisfy` isLeft

      it "should parse without space" $ do
        result <- eval "(\\a -> a + 1 : Int -> Int)"
        result `shouldSatisfy` isRight

      it "should typecheck" $ do
        result <- eval "(\\a -> a + 1 : Int -> Int)"
        result `shouldSatisfy` isRight

      it "should typecheck (and print properly)" $ do
        result <- eval "(\\a -> a : Maybe.Maybe a -> Maybe.Maybe a)"
        result `shouldSatisfy` isRight

      it "should not typecheck if boolean and int do not match" $ do
        result <- eval "(\\a -> a + 1 : Boolean -> Int)"
        result `shouldSatisfy` isLeft

      it "should not typecheck if unifying int with 'a'" $ do
        result <- eval "\\(abc: a) -> abc + 1"
        result `shouldSatisfy` isLeft

      it "should unify named type variables with themselves" $ do
        result <- eval "(\\abc -> \\defVal -> abc == defVal : a -> a -> Boolean)"
        result `shouldSatisfy` isRight

      it "should not unify named type variables with one another" $ do
        result <- eval "(\\abc -> \\defVal -> abc == defVal: a -> b -> Boolean)"
        result `shouldSatisfy` isLeft

      it "should typecheck when id has a specific type" $ do
        result <- eval "let (identity: a -> a) abc = abc; identity True"
        fst <$> result `shouldBe` Right (MTPrim mempty MTBool)

      it "each type variable is unique to the scope it's introduced in" $ do
        result <- eval "let (id1: a -> (a,a)) a = (a,a); let (id2: a -> a) b = b; id1 (id2 True)"
        result `shouldSatisfy` isRight

      it "annotation does not match" $ do
        result <- eval "let (f: a -> String -> a) a b = if True then a else b"
        result `shouldSatisfy` isLeft

      it "annotation does not match (1)" $ do
        result <- eval "(\\a -> \\b -> if True then a else b : String -> a -> String) \"Hi\" \"Lo\""
        result `shouldSatisfy` isLeft

      it "annotation does not match (2)" $ do
        result <- eval "(\\a -> \\b -> if True then a else b : a -> String -> a) \"Hi\" \"Lo\""
        result `shouldSatisfy` isLeft

      it "annotation does not match (3)" $ do
        result <- eval "(\\a -> a : a -> String) \"Hi\""
        result `shouldSatisfy` isLeft

      it "annotation does not match (4)" $ do
        result <- eval "(\\a -> a : String -> a) \"Hi\""
        result `shouldSatisfy` isLeft

    describe "check if" $ do
      it "spots mismatched predicate type" $ do
        result <- eval "let (a: Int) = if 1 then 2 else 3; a"
        result `shouldSatisfy` textErrorContains "Predicate for an if expression"

    describe "check app" $ do
      it "spots function argument does not match with annotated function" $ do
        result <- eval "(\\a -> a: Int -> Int) True"
        result `shouldSatisfy` textErrorContains "Incorrect function argument"

      it "spots function argument does not match" $ do
        result <- eval "(\\a -> a + 1) True"
        result `shouldSatisfy` textErrorContains "Incorrect function argument"

      it "spots function argument does not match when it is used with a variable" $ do
        result <- eval "let f a = a + 1; f True"
        result `shouldSatisfy` textErrorContains "Incorrect function argument"

    describe "optimisations" $ do
      it "should do all optimisations in one pass" $ do
        result <- eval "\\opts -> let d = \"dog\"; match [\"a\", \"b\"] with [a, b, c] -> (Maybe.Just ((a, d))) | _ -> (Maybe.Nothing)"
        result `shouldSatisfy` isRight

    describe "operators" $ do
      it "Bind <<< to compose" $ do
        result <- eval "infix <<< = Prelude.compose; True"
        -- binding to a two arity function is A++
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Bind incrementInt to <<<" $ do
        result <- eval "let incrementInt a = a + 1; infix <<< = incrementInt; True"
        -- we can only bind to a two arity function
        result `shouldSatisfy` isLeft

      it "define +++ as infix and use it" $ do
        result <- eval "let addInt a b = a + b; infix +++ = addInt; 1 +++ 2"
        result `shouldBe` Right (MTPrim mempty MTInt, int 3)

      it "multiple uses of infix with same type" $ do
        result <- eval "let incrementInt a = a + 1; let apply a f = f a; infix |> = apply; 1 |> incrementInt |> incrementInt"
        result `shouldBe` Right (MTPrim mempty MTInt, int 3)

      it "multiple uses of infix with same type 2" $ do
        result <- eval "let incrementInt a = a + 1; let isOne a = a == 1; let apply a f = f a; infix |> = apply; 1 |> incrementInt |> isOne"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "multiple uses of infix with different types" $ do
        result <- eval "let incrementInt a = a + 1; let apply a f = f a; infix |> = apply; 1 |> incrementInt |> State.pure"
        result `shouldSatisfy` isRight

      it "Can't override a built-in infix op" $ do
        result <- eval "infix == = addInt; True"
        -- can't overwrite built in infix operators
        result `shouldSatisfy` isLeft

      it "Binds addInt to +++" $ do
        result <- eval "let addInt a b = a + b; infix +++ = addInt; 1 +++ True"
        -- function typechecking should still work
        result `shouldSatisfy` isLeft

      it "Greater than 1" $ do
        result <- eval "10 > 1"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Greater than 2" $ do
        result <- eval "1 > 10"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "Greater than 3" $ do
        result <- eval "True < 1"
        result `shouldSatisfy` isLeft

      it "Greater than or equal to 1" $ do
        result <- eval "10 >= 1"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Greater than or equal to 2" $ do
        result <- eval "10 >= 10"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Greater than or equal to 3" $ do
        result <- eval "9 >= 10"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "Less than 1" $ do
        result <- eval "1 < 10"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Less than 2" $ do
        result <- eval "10 < 1"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "Less than or equal to 1" $ do
        result <- eval "1 <= 10"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Less than or equal to 2" $ do
        result <- eval "10 <= 10"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Less than or equal to 3" $ do
        result <- eval "10 <= 9"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      describe "Big stuff that breaks interpreter" $ do
        it "Parses using a lexeme" $ do
          let expr =
                mconcat
                  [ "let lexeme p = Parser.left p Parser.space0; ",
                    "let bracketL = lexeme (Parser.char \"[\"); ",
                    "Parser.run bracketL \"[          \""
                  ]
          result <- eval expr
          result `shouldSatisfy` isRight
        it "Parses a JSON array" $ do
          let expr =
                mconcat
                  [ "let lexeme p = Parser.left p Parser.space0; ",
                    "let bracketL = lexeme (Parser.char \"[\"); ",
                    "let bracketR = lexeme (Parser.char \"]\"); ",
                    "let comma = lexeme (Parser.char \",\"); ",
                    "let inner = lexeme (Parser.char \"d\"); ",
                    "let bigP = Parser.right bracketL (Parser.left (Parser.sepBy comma inner) bracketR); ",
                    "Parser.run bigP \"[d,d,d,d]\""
                  ]
          result <- eval expr
          result `shouldSatisfy` isRight
