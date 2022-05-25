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
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Helpers.CheckStoreExpression as Actions
import qualified Language.Mimsa.Actions.Helpers.Parse as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Optimise as Actions
import Language.Mimsa.ExprUtils
import Language.Mimsa.Printer
import Language.Mimsa.Project.Helpers
import Language.Mimsa.Project.Stdlib (buildStdlib)
import Language.Mimsa.Store.Hashing
import Language.Mimsa.Store.Storage (getStoreExpressionHash)
import Language.Mimsa.Transform.FindUnused
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.NormaliseTypes
import Language.Mimsa.Types.AST
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
    saveStoreExpression,
  )

stdlib :: Project Annotation
stdlib = fromRight buildStdlib

eval ::
  Project Annotation ->
  Text ->
  IO (Either Text (Type (), Expr Name ()))
eval env input = do
  let action = do
        expr <- Actions.parseExpr input
        (mt, interpretedExpr, storeExpr, _, _) <- Actions.evaluate input expr
        pure (mt, interpretedExpr, storeExpr)
  case Actions.run env action of
    Right (newPrj, _, (mt, endExpr, se)) -> do
      saveRegressionData (se $> ())
      _ <- optimise newPrj se
      pure (Right (normaliseType (toEmptyType mt), toEmptyAnn endExpr))
    Left e -> pure (Left (prettyPrint e))

optimise ::
  Project Annotation ->
  StoreExpression Annotation ->
  IO (ResolvedExpression Annotation)
optimise prj storeExpr = do
  let action = do
        -- optimise once
        se <- Actions.optimiseStoreExpression storeExpr
        -- optimise twice, it should be the same, if not we need to optimise
        -- more thoroughly
        newSe <- Actions.optimiseStoreExpression se
        if (se $> ()) /= (newSe $> ())
          then do
            error
              ( "Optimising twice gives different results for:\n"
                  <> T.unpack (prettyPrint (storeExpression storeExpr))
                  <> "\nAfter first optimise:\n"
                  <> show (storeExpression se)
                  <> "\nAfter second optimise:\n"
                  <> show (storeExpression newSe)
              )
          else pure ()

        -- have we left some unused vars?
        let stillUnused = findUnused (storeExpression se)
        if not (S.null stillUnused)
          then
            error $
              "Unused found after optimise: "
                <> T.unpack (prettyPrint stillUnused)
                <> "\n"
                <> T.unpack (prettyPrint (storeExpression se))
          else pure ()

        Actions.checkStoreExpression (prettyPrint se) prj se
   in case Actions.run prj action of
        Right (_, _, re) -> pure re
        Left e -> error (T.unpack $ prettyPrint e)

-- These are saved and used in the deserialisation tests to make sure we avoid
-- future regressions
saveRegressionData :: StoreExpression () -> IO ()
saveRegressionData se = do
  jsonPath <- createOutputFolder "StoreExpr"
  let jsonFilename = jsonPath <> show (getStoreExpressionHash se) <> ".json"
  saveStoreExpression jsonFilename se
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
    it "Save testStdlib" $
      saveProject testStdlib
        >> (True `shouldBe` True)
    describe "End to end parsing to evaluation" $ do
      it "let x = ((1,2)) in fst x" $ do
        result <- eval testStdlib "let x = ((1,2)) in fst x"
        result
          `shouldBe` Right
            (MTPrim mempty MTInt, int 1)

      it "let good = { dog: True } in good.dog" $ do
        result <- eval testStdlib "let good = ({ dog: True }) in good.dog"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "let prelude = { id: (\\i -> i) } in prelude.id" $ do
        result <- eval testStdlib "let prelude = ({ id: (\\i -> i) }) in prelude.id"
        result
          `shouldBe` Right
            ( MTFunction mempty (unknown 1) (unknown 1),
              MyLambda mempty (Identifier mempty "i") (MyVar mempty "i")
            )

      it "let prelude = ({ id: (\\i -> i) }) in prelude.id 1" $ do
        result <- eval testStdlib "let prelude = ({ id: (\\i -> i) }) in prelude.id 1"
        result
          `shouldBe` Right
            ( MTPrim mempty MTInt,
              int 1
            )

      it "let bigPrelude = ({ prelude: { id: (\\i -> i) } }) in bigPrelude.prelude.id 1" $ do
        result <- eval testStdlib "let bigPrelude = ({ prelude: { id: (\\i -> i) } }) in bigPrelude.prelude.id 1"
        result
          `shouldBe` Right
            ( MTPrim mempty MTInt,
              int 1
            )

      it "compose incrementInt" $ do
        result <- eval testStdlib "let compose = (\\f -> \\g -> \\a -> f (g a)) in let blah = compose incrementInt incrementInt in blah 67"
        result `shouldBe` Right (MTPrim mempty MTInt, int 69)

      it "let reuse = ({ first: id 1, second: id 2 }) in reuse.first" $ do
        result <- eval testStdlib "let reuse = ({ first: id 1, second: id 2 }) in reuse.first"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)
      it "let id = \\a -> a in id 1" $ do
        result <- eval mempty "let id = \\a -> a in id 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)
      it "let reuse = ({ first: id True, second: id 2 }) in reuse.first" $ do
        result <- eval testStdlib "let reuse = ({ first: id True, second: id 2 }) in reuse.first"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "let reuse = ({ first: id, second: id 2 }) in reuse.first True" $ do
        result <- eval testStdlib "let reuse = ({ first: id, second: id 2 }) in reuse.first True"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "reuses polymorphic function" $ do
        result <- eval testStdlib "let reuse = ({ first: const 1, second: const True }) in reuse.first 100"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)
      it "reuses polymorphic function 2" $ do
        result <- eval testStdlib "let reuse = ({ first: const True, second: const 2 }) in reuse.second 100"
        result `shouldBe` Right (MTPrim mempty MTInt, int 2)
      it "reuses polymorphic function defined here" $ do
        result <- eval testStdlib "let id2 a = a; (id2 1, id2 True)"
        result `shouldSatisfy` isRight
      it "addInt 1 2" $ do
        result <- eval testStdlib "addInt 1 2"
        result `shouldBe` Right (MTPrim mempty MTInt, int 3)
      it "(\\a -> a) 1" $ do
        result <- eval testStdlib "(\\a -> a) 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)
      it "(\\b -> (\\a -> b)) 0 1" $ do
        result <- eval testStdlib "(\\b -> (\\a -> b)) 0 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 0)
      it "addInt 1 (addInt (addInt 2 4) 5)" $ do
        result <- eval testStdlib "addInt 1 (addInt (addInt 2 4) 5)"
        result `shouldBe` Right (MTPrim mempty MTInt, int 12)
      it "type LeBool = Vrai | Faux in Vrai" $ do
        result <- eval testStdlib "type LeBool = Vrai | Faux in Vrai"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty "LeBool" [],
              MyConstructor mempty "Vrai"
            )
      it "type Nat = Zero | Suc Nat in Suc Zero" $ do
        result <- eval testStdlib "type Nat = Zero | Suc Nat in Suc Zero"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty "Nat" [],
              MyApp
                mempty
                (MyConstructor mempty "Suc")
                (MyConstructor mempty "Zero")
            )
      it "type Nat = Zero | Suc Nat in Suc (Suc Zero)" $ do
        result <- eval testStdlib "type Nat = Zero | Suc Nat in Suc (Suc Zero)"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty "Nat" [],
              MyApp
                mempty
                (MyConstructor mempty "Suc")
                ( MyApp
                    mempty
                    (MyConstructor mempty "Suc")
                    (MyConstructor mempty "Zero")
                )
            )
      it "type Nat = Zero | Suc Nat in Suc 1" $ do
        result <- eval testStdlib "type Nat = Zero | Suc Nat in Suc 1"
        result
          `shouldSatisfy` isLeft
      it "type Nat = Zero | Suc Nat in Suc Dog" $ do
        result <- eval testStdlib "type Nat = Zero | Suc Nat in Suc Dog"
        result
          `shouldSatisfy` isLeft
      it "type Nat = Zero | Suc Nat in Suc" $ do
        result <- eval testStdlib "type Nat = Zero | Suc Nat in Suc"
        result
          `shouldBe` Right
            ( MTFunction
                mempty
                (dataTypeWithVars mempty "Nat" [])
                (dataTypeWithVars mempty "Nat" []),
              MyConstructor mempty "Suc"
            )
      it "type OhNat = Zero | Suc OhNat String in Suc" $ do
        result <- eval testStdlib "type OhNat = Zero | Suc OhNat String in Suc"
        result
          `shouldBe` Right
            ( MTFunction
                mempty
                (dataTypeWithVars mempty "OhNat" [])
                ( MTFunction
                    mempty
                    (MTPrim mempty MTString)
                    (dataTypeWithVars mempty "OhNat" [])
                ),
              MyConstructor mempty "Suc"
            )
      it "type Pet = Cat String | Dog String in Cat \"mimsa\"" $ do
        result <- eval testStdlib "type Pet = Cat String | Dog String in Cat \"mimsa\""
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty "Pet" [],
              MyApp
                mempty
                (MyConstructor mempty "Cat")
                (str' "mimsa")
            )
      it "type Void in 1" $ do
        result <- eval testStdlib "type Void in 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)
      it "type String = Should | Error in Error" $ do
        result <- eval testStdlib "type String = Should | Error in Error"
        result `shouldSatisfy` isLeft
      it "type LongBoy = Stuff String Int String in Stuff \"yes\"" $ do
        result <- eval testStdlib "type LongBoy = Stuff String Int String in Stuff \"yes\""
        result
          `shouldBe` Right
            ( MTFunction
                mempty
                (MTPrim mempty MTInt)
                ( MTFunction
                    mempty
                    (MTPrim mempty MTString)
                    (dataTypeWithVars mempty "LongBoy" [])
                ),
              MyApp
                mempty
                (MyConstructor mempty "Stuff")
                (str' "yes")
            )
      it "type Tree = Leaf Int | Branch Tree Tree in Branch (Leaf 1) (Leaf 2)" $ do
        result <- eval testStdlib "type Tree = Leaf Int | Branch Tree Tree in Branch (Leaf 1) (Leaf 2)"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty "Tree" [],
              MyApp
                mempty
                ( MyApp
                    mempty
                    (MyConstructor mempty "Branch")
                    (MyApp mempty (MyConstructor mempty "Leaf") (int 1))
                )
                (MyApp mempty (MyConstructor mempty "Leaf") (int 2))
            )
      it "type Maybe a = Just a | Nothing in Just" $ do
        result <- eval testStdlib "type Maybe a = Just a | Nothing in Just"
        result
          `shouldBe` Right
            ( MTFunction
                mempty
                (MTVar mempty (TVUnificationVar 1))
                (dataTypeWithVars mempty "Maybe" [MTVar mempty (TVUnificationVar 1)]),
              MyConstructor mempty "Just"
            )
      it "type Maybe a = Just a | Nothing in Nothing" $ do
        result <- eval testStdlib "type Maybe a = Just a | Nothing in Nothing"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty "Maybe" [MTVar mempty (TVUnificationVar 1)],
              MyConstructor mempty "Nothing"
            )
      it "type Maybe a = Just a | Nothing in Just 1" $ do
        result <- eval testStdlib "type Maybe a = Just a | Nothing in Just 1"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty "Maybe" [MTPrim mempty MTInt],
              MyApp
                mempty
                (MyConstructor mempty "Just")
                (int 1)
            )

      it "use Maybe with eq" $ do
        result <- eval testStdlib "type Maybe a = Just a | Nothing in match Just 1 with (Just a) -> eq 100 a | Nothing -> False"
        result
          `shouldBe` Right
            (MTPrim mempty MTBool, bool False)

      it "type Maybe a = Just a | Nothing in match Just 1 with (Just a) -> True | Nothing -> 1" $ do
        result <- eval testStdlib "type Maybe a = Just a | Nothing in match Just 1 with (Just a) -> True | Nothing -> 1"
        result `shouldSatisfy` isLeft

      it "unfolding Maybe more" $ do
        result <- eval testStdlib "type Maybe a = Just a | Nothing in match Just 1 with (Just a) -> eq 100 a | _ -> False"
        result
          `shouldBe` Right
            (MTPrim mempty MTBool, bool False)

      it "Extracts values with pattern match" $ do
        result <- eval testStdlib "type Stuff = Thing String Int in match Thing \"Hello\" 1 with (Thing name num) -> name"
        result
          `shouldBe` Right
            (MTPrim mempty MTString, str' "Hello")

      it "type Result e a = Failure e | Success a in match Failure \"oh no\" with (Success a) -> \"oh yes\" | (Failure e) -> e" $ do
        result <- eval testStdlib "type Result e a = Failure e | Success a in match Failure \"oh no\" with (Success a) -> \"oh yes\" | (Failure e) -> e"
        result
          `shouldBe` Right
            (MTPrim mempty MTString, str' "oh no")

      it "type Blap a = Boop a Int in match Boop True 100 with (Boop a b) -> a" $ do
        result <- eval testStdlib "type Blap a = Boop a Int in match Boop True 100 with (Boop a b) -> a"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "type Maybe a = Just a | Nothing in match Nothing with Nothing False" $ do
        result <- eval testStdlib "type Maybe a = Just a | Nothing in match Nothing with Nothing False"
        result `shouldSatisfy` isLeft

      it "type Thing = Thing String in let a = Thing \"string\" in match a with (Thing s) -> s" $ do
        result <- eval testStdlib "type Thing = Thing String in let a = Thing \"string\" in match a with (Thing s) -> s"
        result `shouldBe` Right (MTPrim mempty MTString, str' "string")
      it "type Pair a b = Pair a b in match Pair \"dog\" 1 with Pair \a -> a" $ do
        result <- eval testStdlib "type Pair a b = Pair a b in match Pair \"dog\" 1 with Pair \a -> a"
        result `shouldSatisfy` isLeft
      it "type Tree a = Leaf a | Branch (Tree a) (Tree a) in Leaf 1" $ do
        result <- eval testStdlib "type Tree a = Leaf a | Branch (Tree a) (Tree a) in Leaf 1"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty "Tree" [MTPrim mempty MTInt],
              MyApp
                mempty
                (MyConstructor mempty "Leaf")
                (int 1)
            )
      it "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Leaf 1" $ do
        result <- eval testStdlib "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Leaf 1"
        result
          `shouldSatisfy` isLeft
      it "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Branch (Leaf 1) (Leaf True)" $ do
        result <- eval testStdlib "type Tree a = Leaf a | Branch (Tree a) (Tree b) in Branch (Leaf 1) (Leaf True)"
        result
          `shouldSatisfy` isLeft
      it "type Tree a = Empty | Branch (Tree a) a (Tree a) in Branch (Empty) 1 (Empty)" $ do
        result <- eval testStdlib "type Tree a = Empty | Branch (Tree a) a (Tree a) in Branch (Empty) 1 (Empty)"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty "Tree" [MTPrim mempty MTInt],
              MyApp
                mempty
                ( MyApp
                    mempty
                    ( MyApp
                        mempty
                        (MyConstructor mempty "Branch")
                        (MyConstructor mempty "Empty")
                    )
                    (int 1)
                )
                (MyConstructor mempty "Empty")
            )

      it "unwrapping Maybe" $ do
        result <- eval testStdlib "type Maybe a = Just a | Nothing in match Just True with Just \\a -> a | Nothing \"what\""
        result `shouldSatisfy` isLeft

      it "unwrap for Either" $ do
        result <- eval testStdlib "type Either e a = Left e | Right a in \\f -> \\g -> \\either -> match either with (Left e) -> g e | (Right a) -> (f a)"
        result `shouldSatisfy` isRight
      {-
            it "type Maybe a = Just a | Nothing in \\maybe -> match maybe with Just \\a -> a | Nothing \"poo\"" $ do
              result <- eval testStdlib "type Maybe a = Just a | Nothing in \\maybe -> match maybe with Just \\a -> a | Nothing \"poo\""
              fst <$> result
                `shouldBe` Right
                  ( MTFunction (dataTypeWithVars ( "Maybe") []) (MTPrim MTString)
                  )

      -}
      it "type Array a = Empty | Item a (Array a) in match (Item 1 (Item 2 Empty)) with Empty -> Empty | (Item a rest) -> rest" $ do
        result <- eval testStdlib "type Array a = Empty | Item a (Array a) in match (Item 1 (Item 2 Empty)) with Empty -> Empty | (Item a rest) -> rest"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty "Array" [MTPrim mempty MTInt],
              MyApp
                mempty
                ( MyApp
                    mempty
                    ( MyConstructor
                        mempty
                        "Item"
                    )
                    (int 2)
                )
                (MyConstructor mempty "Empty")
            )
      it "let loop = (\\a -> if eq 10 a then a else loop (addInt a 1)) in loop 1" $ do
        result <- eval testStdlib "let loop = (\\a -> if eq 10 a then a else loop (addInt a 1)) in loop 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 10)
      it "type Nat = Zero | Suc Nat in let loop = (\\as -> match as with Zero -> 0 | (Suc as2) -> incrementInt (loop as2)) in loop (Suc (Suc (Suc Zero)))" $ do
        result <- eval testStdlib "type Nat = Zero | Suc Nat in let loop = (\\as -> match as with Zero -> 0 | (Suc as2) -> incrementInt (loop as2)) in loop (Suc (Suc (Suc Zero)))"
        result `shouldBe` Right (MTPrim mempty MTInt, int 3)
      it "type Nat = Zero | Suc Nat in let loop = (\\as -> \\b -> match as with Zero -> b | (Suc as2) -> incrementInt (loop as2 b)) in loop (Suc (Suc (Suc Zero))) 10" $ do
        result <- eval testStdlib "type Nat = Zero | Suc Nat in let loop = (\\as -> \\b -> match as with Zero -> b | (Suc as2) -> incrementInt (loop as2 b)) in loop (Suc (Suc (Suc Zero))) 10"
        result `shouldBe` Right (MTPrim mempty MTInt, int 13)
      {-
            it "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA(addInt(b)(a))(rest)) in reduceA(0)(Item 3 Empty)" $ do
              result <- eval testStdlib "type Arr a = Empty | Item a (Arr a) in let reduceA = (\\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA(addInt(b)(a))(rest)) in reduceA(0)(Item 3 Empty)"
              result `shouldBe` Right (MTPrim mempty MTInt, int 3)
      -}
      it "type Array a = Empty | Item a (Array a) in let reduceA = (\\f -> \\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA f (f b a) rest) in reduceA addInt 0 Empty" $ do
        result <- eval testStdlib "type Array a = Empty | Item a (Array a) in let reduceA = (\\f -> \\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA f (f b a) rest) in reduceA addInt 0 Empty"
        result `shouldBe` Right (MTPrim mempty MTInt, int 0)
      it "type Array a = Empty | Item a (Array a) in let reduceA = (\\f -> \\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA f (f b a) rest) in reduceA addInt 0 (Item 3 Empty)" $ do
        result <- eval testStdlib "type Array a = Empty | Item a (Array a) in let reduceA = (\\f -> \\b -> \\as -> match as with Empty -> b | (Item a rest) -> reduceA f (f b a) rest) in reduceA addInt 0 (Item 3 Empty)"
        result `shouldBe` Right (MTPrim mempty MTInt, int 3)
      it "type Tlee a = Non | Tlee (Maybe b) in {}" $ do
        result <- eval testStdlib "type Tlee a = Non | Tlee (Maybe b) in {}"
        result `shouldSatisfy` isLeft
      it "let some = \\a -> Just a in if True then some 1 else Nothing" $ do
        result <- eval testStdlib "let some = \\a -> Just a in if True then some 1 else Nothing"
        result
          `shouldBe` Right
            ( dataTypeWithVars mempty "Maybe" [MTPrim mempty MTInt],
              MyApp
                mempty
                (MyConstructor mempty "Just")
                (int 1)
            )
      it "\\a -> match a with (Just as) -> True | Nothing -> 100" $ do
        result <- eval testStdlib "\\a -> match a with (Just as) -> True | Nothing -> 100"
        fst <$> result
          `shouldSatisfy` isLeft
      it "\\a -> match a with (Just as) -> as | Nothing -> 100" $ do
        result <- eval testStdlib "\\a -> match a with (Just as) -> as | Nothing -> 100"
        fst <$> result
          `shouldBe` Right
            ( MTFunction
                mempty
                (dataTypeWithVars mempty "Maybe" [MTPrim mempty MTInt])
                (MTPrim mempty MTInt)
            )
      it "fromMaybe should fail typecheck when default does not match inner value" $ do
        result <- eval testStdlib "let fromMaybe = \\defVal -> (\\maybe -> match maybe with (Just a) -> a | Nothing -> defVal) in fromMaybe \"Horse\" (Just 1)"
        result `shouldSatisfy` isLeft
      it "fromMaybe works when types match up" $ do
        result <- eval testStdlib "let fromMaybe = \\defVal -> (\\maybe -> match maybe with (Just a) -> a | Nothing -> defVal) in fromMaybe \"Horse\" (Just \"Dog\")"
        result `shouldBe` Right (MTPrim mempty MTString, str' "Dog")
      it "True == \"dog\"" $ do
        result <- eval testStdlib "True == \"dog\""
        result `shouldSatisfy` isLeft
      it "(\\a -> a) == (\\b -> b)" $ do
        -- no function equality
        result <- eval testStdlib "(\\a -> a) == (\\b -> b)"
        result `shouldSatisfy` isLeft
      it "True == False" $ do
        result <- eval testStdlib "True == False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)
      it "True == True" $ do
        result <- eval testStdlib "True == True"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "(Just 1) == Just 2" $ do
        result <- eval testStdlib "(Just 1) == Just 2"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)
      it "let eq1 = (\\a -> a == 1) in eq1 1" $ do
        result <- eval testStdlib "let eq1 = (\\a -> a == 1) in eq1 1"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "1 + 1" $ do
        result <- eval testStdlib "1 + 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 2)
      it "True + 1" $ do
        result <- eval testStdlib "True + 1"
        result `shouldSatisfy` isLeft
      it "10 - 1" $ do
        result <- eval testStdlib "10 - 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 9)

      it "True - 1" $ do
        result <- eval testStdlib "True - 1"
        result `shouldSatisfy` isLeft

      it "1 + 1 + 1 + 1" $ do
        result <- eval testStdlib "1 + 1 + 1 + 1"
        result `shouldBe` Right (MTPrim mempty MTInt, int 4)

      it "\"dog\" ++ \"log\"" $ do
        result <- eval testStdlib "\"dog\" ++ \"log\""
        result `shouldBe` Right (MTPrim mempty MTString, str' "doglog")

      it "\"dog\" ++ 123" $ do
        result <- eval testStdlib "\"dog\" ++ 123"
        result `shouldSatisfy` isLeft

      it "passes record to function" $ do
        result <- eval testStdlib "let f = (\\a -> if True then a.num else a.num2) in f {num: 1, num2: 2}"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)

      it "if True then { one: 1 } else { two: 2 }" $ do
        result <- eval testStdlib "if True then { one: 1 } else { two: 2 }"
        result `shouldSatisfy` isLeft

      it "if True then { one: 1 } else { one: 2 }" $ do
        result <- eval testStdlib "if True then { one: 1 } else { one: 2 }"
        result `shouldSatisfy` isRight

      it "let a = { one: 1 }; let one = a.one; let two = a.two; a" $ do
        result <- eval testStdlib "let a = { one: 1 }; let one = a.one; let two = a.two; a"
        result `shouldSatisfy` isLeft

      it "\\a -> let one = a.one; let two = a.two; a" $ do
        result <- eval testStdlib "\\a -> let one = a.one; let two = a.two; a"
        result
          `shouldSatisfy` isRight

      it "passes inferred record value to a function" $ do
        result <- eval testStdlib "let useRecord = (\\a -> let one = a.one; let two = a.two; one + two) in useRecord {one: 1}"
        result `shouldSatisfy` \case
          (Left err) -> not $ T.isInfixOf "InterpreterError" err
          _ -> False

      it "passes inferred value to another function" $ do
        result <- eval testStdlib "let useRecord = (\\a -> let one = a.one; let two = a.two; one + two) in useRecord {two: 2}"
        result `shouldSatisfy` \case
          (Left err) -> not $ T.isInfixOf "InterpreterError" err
          _ -> False

      it "passes complete record value to function" $ do
        result <- eval testStdlib "let useRecord = (\\a -> let one = a.one; let two = a.two; one + two) in useRecord {one: 1, two: 2}"
        result `shouldSatisfy` isRight

      it "if ?missingFn then 1 else 2" $ do
        result <- eval testStdlib "if ?missingFn then 1 else 2"
        result `shouldSatisfy` \case
          (Left msg) ->
            T.isInfixOf "Typed holes found" msg
              && T.isInfixOf "?missingFn" msg
              && T.isInfixOf "Boolean" msg
          (Right _) -> False

      it "typed holes found in map function" $ do
        result <- eval testStdlib "let map = \\f -> \\a -> f a in map ?flappy 1"
        result `shouldSatisfy` \case
          (Left msg) ->
            T.isInfixOf "Typed holes found" msg
              && T.isInfixOf "^^^^^^^" msg
          (Right _) -> False

      it "compose function" $ do
        result <- eval testStdlib "let compose = \\f -> \\g -> \\a -> f (g a); compose"
        result `shouldSatisfy` isRight

      it "Just (1 == 1)" $ do
        result <- eval testStdlib "Just (1 == 1)"
        snd <$> result
          `shouldBe` Right
            ( MyApp
                mempty
                (MyConstructor mempty "Just")
                (bool True)
            )
      it "\\a -> if (100 == a.int) then 100 else 0" $ do
        result <- eval testStdlib "\\a -> if (100 == a.int) then 100 else 0"
        result `shouldSatisfy` isRight
      it "\\a -> if (a.one == a.two) then 100 else 0" $ do
        result <- eval testStdlib "\\a -> if (a.one == a.two) then 100 else 0"
        result `shouldSatisfy` isRight
      it "type Reader r a = Reader (r -> a) in Reader (\\r -> r + 100)" $ do
        result <- eval testStdlib "type Reader r a = Reader (r -> a) in Reader (\\r -> r + 100)"
        result
          `shouldBe` Right
            ( dataTypeWithVars
                mempty
                "Reader"
                [MTPrim mempty MTInt, MTPrim mempty MTInt],
              MyApp
                mempty
                (MyConstructor mempty "Reader")
                ( MyLambda
                    mempty
                    (Identifier mempty "r")
                    ( MyInfix
                        mempty
                        Add
                        (MyVar mempty "r")
                        (int 100)
                    )
                )
            )

      it "\\state -> \\s -> match state with (State sas) -> sas s" $ do
        result <- eval testStdlib "\\state -> \\s -> match state with (State sas) -> sas s"
        result `shouldSatisfy` isRight

      it "let a = pureState \"dog\"; let b = bindState storeName a; runState b nil" $ do
        result <- eval testStdlib "let a = pureState \"dog\"; let b = bindState storeName a; runState b nil"
        result `shouldSatisfy` isRight

      it "let a = pureState \"dog\"; let b = bindState storeName a; let c = bindState storeName b; runState c nil" $ do
        result <- eval testStdlib "let a = pureState \"dog\"; let b = bindState storeName a; let c = bindState storeName b; runState c nil"
        result `shouldSatisfy` isRight

      it "Stops boolean and Maybe<A> being used together" $ do
        result <- eval testStdlib "\\some -> match some with (Just a) -> Just (a == 1) | _ -> some"
        result `shouldSatisfy` isLeft

      -- need a new way of stopping this looping
      -- perhaps static analysis for silly cases
      -- and a timeout when used as an endpoint?
      xit "Interpreter is stopped before it loops infinitely" $ do
        result <- eval testStdlib "let forever = \\a -> forever a in forever True"
        result `shouldSatisfy` \case
          Left msg -> "interpreter aborted" `T.isInfixOf` msg
          _ -> False

      -- built-ins should not be used as type constructors
      it "type Justthing = String in True" $ do
        result <- eval testStdlib "type Justthing = String in True"
        result `shouldSatisfy` isLeft

      it "type Pair a b = Pair (a,b)" $ do
        result <- eval testStdlib "type Pair a b = Pair (a,b) in True"
        result `shouldSatisfy` isRight

      it "type Record a = Record { name: String, other: a } in True" $ do
        result <- eval testStdlib "type Record a = Record { name: String, other: a } in True"
        result `shouldSatisfy` isRight

      it "type State s a = State (s -> (a,s)) in True" $ do
        result <- eval testStdlib "type State s a = State (s -> (a,s)) in True"
        result `shouldSatisfy` isRight

      it "\\person -> match person with (Person p) -> p.age" $ do
        result <- eval testStdlib "\\person -> match person with (Person p) -> p.age"
        result `shouldSatisfy` isRight

      -- simplest swaps test
      it "\\a -> 1" $ do
        result <- eval mempty "\\a -> 1"
        case result of
          Left _ -> error "Was not supposed to fail"
          Right (_, expr') -> T.unpack (prettyPrint expr') `shouldContain` "a"

      it "filter function for strings" $ do
        result <- eval testStdlib "let filter = \\pred -> \\str -> let fn = (\\s -> match s with a ++ as -> let rest = fn as; if pred a then a ++ rest else rest | _ -> \"\") in fn str; filter (\\a -> a == \"o\") \"woo\""
        result
          `shouldBe` Right
            ( MTPrim mempty MTString,
              MyLiteral mempty (MyString "oo")
            )

      it "runParser anyChar \"dog\"" $ do
        result <- eval testStdlib "runParser anyChar \"dog\""
        result `shouldSatisfy` isRight

      it "fmapParser works correctly" $ do
        result <- eval testStdlib "let repeat = fmapParser (\\a -> a ++ a) anyChar in runParser repeat \"dog\""
        snd <$> result
          `shouldBe` Right
            ( MyApp
                mempty
                (MyConstructor mempty "Just")
                (MyLiteral mempty (MyString "dd"))
            )

      it "bindParser works correctly" $ do
        result <- eval testStdlib "let parser = bindParser (\\a -> if a == \"d\" then anyChar else failParser) anyChar; runParser parser \"dog\""
        snd <$> result
          `shouldBe` Right
            ( MyApp
                mempty
                (MyConstructor mempty "Just")
                (MyLiteral mempty (MyString "o"))
            )

      it "bindParser fails correctly" $ do
        result <- eval testStdlib "let parser = bindParser (\\a -> if a == \"d\" then anyChar else failParser) anyChar; runParser parser \"log\""
        snd <$> result
          `shouldBe` Right
            (MyConstructor mempty "Nothing")

      it "apParser formats correctly" $ do
        result <- eval testStdlib "\\parserF -> \\parserA -> let (Parser pF) = parserF; let (Parser pA) = parserA; Parser (\\input -> match (pF input) with Just (f, input2) -> (match (pA input2) with Just (a, input3) -> Just (f a, input3) | _ -> Nothing) | _ ->  Nothing)"
        result `shouldSatisfy` isRight

      it "Array literal" $ do
        result <- eval testStdlib "[1,2,3]"
        result
          `shouldBe` Right
            ( MTArray mempty (MTPrim mempty MTInt),
              MyArray mempty [int 1, int 2, int 3]
            )

      it "[1,True,3]" $ do
        result <- eval testStdlib "[1,True,3]"
        result
          `shouldSatisfy` isLeft

    describe "Native array" $ do
      it "[1] <> [2]" $ do
        result <- eval testStdlib "[1] <> [2]"
        result
          `shouldBe` Right
            ( MTArray mempty (MTPrim mempty MTInt),
              MyArray mempty [int 1, int 2]
            )
      it "[1] <> [True]" $ do
        result <- eval testStdlib "[1] <> [True]"
        result `shouldSatisfy` isLeft
      it "[1] <> \"2\"" $ do
        result <- eval testStdlib "[1] <> \"2\""
        result
          `shouldSatisfy` isLeft
      it "\"1\" <> [2]" $ do
        result <- eval testStdlib "\"1\" <> [2]"
        result
          `shouldSatisfy` isLeft
      it "mapArray (\\a -> a + 1) [1,2,3]" $ do
        result <- eval testStdlib "mapArray (\\a -> a + 1) [1,2,3]"
        result
          `shouldBe` Right
            ( MTArray mempty (MTPrim mempty MTInt),
              MyArray mempty [int 2, int 3, int 4]
            )
    describe "Let pattern" $ do
      it "Matches a wildcard" $ do
        result <- eval testStdlib "let _ = False in True"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Matches a value" $ do
        result <- eval testStdlib "let a = True in a"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Matches a pair" $ do
        result <- eval testStdlib "let (a,b) = (True,False) in a"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Matches a record" $ do
        result <- eval testStdlib "let { dog: a } = { dog: True } in a"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Does not match a constructor with other cases" $ do
        result <- eval testStdlib "let (Just a) = Just True in a"
        result `shouldSatisfy` isLeft
      it "Matches a one case constructor" $ do
        result <- eval testStdlib "let (Ident a) = Ident True in a"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Matches a nested one case constructor" $ do
        result <- eval testStdlib "let (Ident (Ident a)) = Ident (Ident True) in a"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Does not matches a pair that is not complete" $ do
        result <- eval testStdlib "let (a,True) = (True,False) in a"
        result `shouldSatisfy` isLeft
      it "Adds constructors to required types for StoreExpression" $ do
        result <- eval testStdlib "let (Parser parser) = predParser (\\d -> d == \"d\") anyChar in parser \"dog\""
        result `shouldSatisfy` isRight
    describe "Pattern matching" $ do
      it "Matches a wildcard" $ do
        result <- eval testStdlib "match 1 with _ -> True"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Matches a variable" $ do
        result <- eval testStdlib "match 1 with a -> a"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)
      it "Deconstructs a pair" $ do
        result <- eval testStdlib "match (1,True) with (a,b) -> b"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Matches an int literal" $ do
        result <- eval testStdlib "match (1, True) with (1, a) -> a | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Matches a string literal" $ do
        result <- eval testStdlib "match \"dog\" with \"dog\" -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Matches two string literals" $ do
        result <- eval testStdlib "match \"dog\" with \"dog\" -> True | \"log\" -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Matches a record" $ do
        result <- eval testStdlib "match { dog: 1 } with { dog: a } -> a"
        result `shouldBe` Right (MTPrim mempty MTInt, int 1)
      it "Matches a constructor with no args" $ do
        result <- eval testStdlib "match Nothing with Nothing -> False | _ -> True"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)
      it "Matches a constructor with args" $ do
        result <- eval testStdlib "match Just 1 with (Just _) -> True | Nothing -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Matches These correctly" $ do
        result <- eval testStdlib "match This 1 with (These _ _) -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)
      it "Fallbacks to correct catchall" $ do
        result <- eval testStdlib "match Just 1 with (Just _) -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Typechecks Either correctly" $ do
        result <- eval testStdlib "match Right 100 with (Left \"log\") -> False | (Right 100) -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Does not have a swap error" $ do
        result <- eval testStdlib "\\a -> match (Left a) with (Left e) -> e | _ -> False"
        result `shouldSatisfy` isRight
      it "Pulls Left into scope from Project" $ do
        result <- eval testStdlib "\\a -> match a with (Left e) -> e | _ -> False"
        result `shouldSatisfy` isRight
      it "Parses constructor application in expr" $ do
        result <- eval testStdlib "match Just 1 with (Just a) -> Just a | _ -> Nothing"
        result `shouldSatisfy` isRight
      it "Parses and pretty prints more complex matches" $ do
        result <- eval testStdlib "\\mf -> \\ma -> match (mf, ma) with (Right f, Right a) -> Right (f a) | (Left e, _) -> Left e | (_, Left e) -> Left e"
        result `shouldSatisfy` isRight
      it "Matches array with non-empty match" $ do
        result <- eval testStdlib "match [1] with [_] -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Matches empty array with empty case" $ do
        result <- eval testStdlib "match [] with [_] -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)
      it "Should not match when input array is longer than pattern" $ do
        result <- eval testStdlib "match [1,2] with [_] -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)
      it "Should match when input is long but we have a SpreadWildcard at the end" $ do
        result <- eval testStdlib "match [1,2,3] with [1,2,...] -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)
      it "Shouldn't match when input is too short with SpreadWildcard at the end" $ do
        result <- eval testStdlib "match [1] with [1,2,...] -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)
      it "Binds empty remainder with array to SpreadValue" $ do
        result <- eval testStdlib "match [1] with [1,...a] -> a | _ -> [0]"
        result
          `shouldBe` Right
            ( MTArray mempty (MTPrim mempty MTInt),
              MyArray mempty mempty
            )
      it "Binds remainder with array to SpreadValue" $ do
        result <- eval testStdlib "match [1,2,3] with [1,...a] -> a | _ -> []"
        result
          `shouldBe` Right
            ( MTArray mempty (MTPrim mempty MTInt),
              MyArray mempty [int 2, int 3]
            )
      it "Errors if we bind the same variable twice" $ do
        result <- eval testStdlib "match (1,2) with (a,a) -> a"
        result `shouldSatisfy` isLeft
      it "Uses a constructor inside an array" $ do
        result <- eval testStdlib "match [] with [Just 1] -> True | _ -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)
      it "Generates more nuanced exhaustiveness checks when using spread operatpr" $ do
        result <- eval testStdlib "match [] with [] -> True | [_] -> False | [_,...] -> False"
        result
          `shouldBe` Right
            (MTPrim mempty MTBool, bool True)
      it "Matches an empty string" $ do
        result <- eval testStdlib "match \"\" with _ ++ _ -> True | \"\" -> False"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)
      it "Matches an non-empty string" $ do
        result <- eval testStdlib "match \"dog\" with a ++ b -> (a,b) | \"\" -> (\"\", \"\")"
        result
          `shouldBe` Right
            ( MTPair mempty (MTPrim mempty MTString) (MTPrim mempty MTString),
              MyPair mempty (MyLiteral mempty (MyString "d")) (MyLiteral mempty (MyString "og"))
            )
      it "Fix empty pattern match obscuring bindings" $ do
        result <- eval testStdlib "\\a -> match Nothing with (Nothing) -> a | _ -> a"
        result `shouldSatisfy` isRight
    describe "Error with List type" $ do
      it "Is fine with no shadowed variables" $ do
        let input =
              mconcat
                [ "\\a -> \\b -> match (a, b) with ",
                  "(Cons aa restA, Nil) -> (Cons aa restA)",
                  " | (Nil, Cons bb restB) -> (Cons bb restB)",
                  " | _ -> (Nil)"
                ]
        result <- eval testStdlib input
        result `shouldSatisfy` isRight
      it "Is fine with shadowed variables" $ do
        let input =
              mconcat
                [ "\\a -> \\b -> match (a, b) with ",
                  "(Cons a restA, Nil) -> (Cons a restA)",
                  " | (Nil, Cons b restB) -> (Cons b restB)",
                  " | _ -> a"
                ]
        result <- eval testStdlib input
        result `shouldSatisfy` isRight
    describe "Too many generics in stringReduce" $ do
      it "simpler type" $ do
        let input = "\\a -> match a with head ++ tail -> head | _ -> \"\""
        result <- eval testStdlib input
        fst <$> result `shouldBe` Right (MTFunction () (MTPrim () MTString) (MTPrim () MTString))
      it "type of function" $ do
        let input = "stringReduce"
        result <- eval testStdlib input
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
      it "Attempt to construct broken MonoPair" $ do
        result <- eval testStdlib "MonoPair (\\a -> a ++ a) (\\b -> b <> b)"
        result `shouldSatisfy` isLeft
      -- skipping because not sure what the hell is going on here
      it "maybeMonoid stringMonoid" $ do
        result <- eval testStdlib "maybeMonoid stringMonoid"
        fst <$> result
          `shouldBe` Right
            ( dataTypeWithVars
                mempty
                "Monoid"
                [ dataTypeWithVars mempty "Maybe" [MTPrim mempty MTString]
                ]
            )
      it "maybeMonoid sumMonoid" $ do
        result <- eval testStdlib "maybeMonoid sumMonoid"
        fst <$> result
          `shouldBe` Right
            ( dataTypeWithVars
                mempty
                "Monoid"
                [ dataTypeWithVars mempty "Maybe" [MTPrim mempty MTInt]
                ]
            )

    describe "Tree interpreter error" $ do
      let leaf = MyApp mempty (MyConstructor mempty "Leaf")
          branch l a =
            MyApp
              mempty
              ( MyApp
                  mempty
                  ( MyApp
                      mempty
                      (MyConstructor mempty "Branch")
                      l
                  )
                  a
              )
      it "Reverses a leaf" $ do
        result <- eval testStdlib "invertTree (Leaf 1)"
        snd <$> result
          `shouldBe` Right (leaf (int 1))

      it "Reverses a branch" $ do
        result <- eval testStdlib "invertTree (Branch (Leaf 1) 2 (Leaf 3))"
        snd <$> result
          `shouldBe` Right (branch (leaf (int 3)) (int 2) (leaf (int 1)))

      it "Reverses a small tree correctly" $ do
        -- should be Branch (Branch (Leaf 5) 4 (Leaf 3)) 2 (Leaf 1)
        result <- eval testStdlib "invertTree (Branch (Leaf 1) 2 (Branch (Leaf 3) 4 (Leaf 5)))"
        snd <$> result
          `shouldBe` Right (branch (branch (leaf (int 5)) (int 4) (leaf (int 3))) (int 2) (leaf (int 1)))

      it "Reversing a tree twice is identity" $ do
        result <- eval testStdlib "let tree = Branch (Leaf 1) 2 (Branch (Leaf 3) 4 (Leaf 5)); invertTree (invertTree tree) == tree"
        snd <$> result
          `shouldBe` Right (bool True)

    describe "delays arity check for infix operators" $ do
      it "is fine" $ do
        result <- eval testStdlib "let flip f a b = f b a; let and a b = if a then b else False; infix <<>> = flip and; True <<>> False"
        result `shouldSatisfy` isRight

    describe "let with type annotation" $ do
      it "should not parse without brackets" $ do
        result <- eval testStdlib "let a: Boolean = True in a"
        result `shouldSatisfy` isLeft

      it "should typecheck" $ do
        result <- eval testStdlib "let (a: Boolean) = True in a"
        result `shouldSatisfy` isRight

      it "should typecheck (with brackets)" $ do
        result <- eval testStdlib "let (a: Boolean) = True in a"
        result `shouldSatisfy` isRight

      it "should not typecheck" $ do
        result <- eval testStdlib "let (a: Int) = True in a"
        result `shouldSatisfy` isLeft

      it "should break with non-existent type" $ do
        result <- eval testStdlib "let (a: FooBar) = True in a"
        result `shouldSatisfy` textErrorContains "Cannot match Boolean and FooBar"

      it "cannot assign concrete value to polymorphic type" $ do
        result <- eval testStdlib "let (a: anyA) = True in a"
        result `shouldSatisfy` isLeft

    describe "regressions" $ do
      it "should destructure record correctly" $ do
        result <- eval testStdlib "let { b: b } = { a: 5, b: 100 } in b"
        result `shouldBe` Right (MTPrim mempty MTInt, int 100)
      it "should show pattern match exhaustiveness error" $ do
        result <- eval testStdlib "let matcher a = match a with [] -> True; matcher [1]"
        result `shouldSatisfy` textErrorContains "Pattern match is not exhaustive"

    describe "lambda with type annotation" $ do
      it "should not parse without brackets" $ do
        result <- eval testStdlib "\\a -> a + 1 : Int -> Int"
        result `shouldSatisfy` isLeft

      it "should parse without space" $ do
        result <- eval testStdlib "(\\a -> a + 1 : Int -> Int)"
        result `shouldSatisfy` isRight

      it "should typecheck" $ do
        result <- eval testStdlib "(\\a -> a + 1 : Int -> Int)"
        result `shouldSatisfy` isRight

      it "should typecheck (and print properly)" $ do
        result <- eval testStdlib "(\\a -> a : Maybe a -> Maybe a)"
        result `shouldSatisfy` isRight

      it "should not typecheck if boolean and int do not match" $ do
        result <- eval testStdlib "(\\a -> a + 1 : Boolean -> Int)"
        result `shouldSatisfy` isLeft

      it "should not typecheck if unifying int with 'a'" $ do
        result <- eval testStdlib "\\(abc: a) -> abc + 1"
        result `shouldSatisfy` isLeft

      it "should unify named type variables with themselves" $ do
        result <- eval testStdlib "(\\abc -> \\defVal -> abc == defVal : a -> a -> Boolean)"
        result `shouldSatisfy` isRight

      it "should not unify named type variables with one another" $ do
        result <- eval testStdlib "(\\abc -> \\defVal -> abc == defVal: a -> b -> Boolean)"
        result `shouldSatisfy` isLeft

      it "should typecheck when id has a specific type" $ do
        result <- eval testStdlib "let (identity: a -> a) abc = abc; identity True"
        fst <$> result `shouldBe` Right (MTPrim mempty MTBool)

      it "each type variable is unique to the scope it's introduced in" $ do
        result <- eval testStdlib "let (id1: a -> (a,a)) a = (a,a); let (id2: a -> a) b = b; id1 (id2 True)"
        result `shouldSatisfy` isRight

      it "annotation does not match" $ do
        result <- eval testStdlib "let (f: a -> String -> a) a b = if True then a else b"
        result `shouldSatisfy` isLeft

      it "annotation does not match (1)" $ do
        result <- eval testStdlib "(\\a -> \\b -> if True then a else b : String -> a -> String) \"Hi\" \"Lo\""
        result `shouldSatisfy` isLeft

      it "annotation does not match (2)" $ do
        result <- eval testStdlib "(\\a -> \\b -> if True then a else b : a -> String -> a) \"Hi\" \"Lo\""
        result `shouldSatisfy` isLeft

      it "annotation does not match (3)" $ do
        result <- eval testStdlib "(\\a -> a : a -> String) \"Hi\""
        result `shouldSatisfy` isLeft

      it "annotation does not match (4)" $ do
        result <- eval testStdlib "(\\a -> a : String -> a) \"Hi\""
        result `shouldSatisfy` isLeft

    describe "check if" $ do
      it "spots mismatched predicate type" $ do
        result <- eval testStdlib "let (a: Int) = if 1 then 2 else 3; a"
        result `shouldSatisfy` textErrorContains "Predicate for an if expression"

    describe "check app" $ do
      it "spots function argument does not match with annotated function" $ do
        result <- eval testStdlib "(\\a -> a: Int -> Int) True"
        result `shouldSatisfy` textErrorContains "Incorrect function argument"

      it "spots function argument does not match" $ do
        result <- eval testStdlib "(\\a -> a + 1) True"
        result `shouldSatisfy` textErrorContains "Incorrect function argument"

      it "spots function argument does not match when it is used with a variable" $ do
        result <- eval testStdlib "let f a = a + 1; f True"
        result `shouldSatisfy` textErrorContains "Incorrect function argument"

    describe "optimisations" $ do
      it "should do all optimisations in one pass" $ do
        result <- eval testStdlib "\\opts -> let d = \"dog\"; match [\"a\", \"b\"] with [a, b, c] -> (Just ((a, d))) | _ -> (Nothing)"
        result `shouldSatisfy` isRight

    describe "operators" $ do
      it "infix <<< = compose; True" $ do
        result <- eval testStdlib "infix <<< = compose; True"
        -- binding to a two arity function is A++
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "infix <<< = incrementInt; True" $ do
        result <- eval testStdlib "infix <<< = incrementInt; True"
        -- we can only bind to a two arity function
        result `shouldSatisfy` isLeft

      it "define +++ as infix and use it" $ do
        result <- eval testStdlib "infix +++ = addInt; 1 +++ 2"
        result `shouldBe` Right (MTPrim mempty MTInt, int 3)

      it "multiple uses of infix with same type" $ do
        result <- eval testStdlib "let apply a f = f a; infix |> = apply; 1 |> incrementInt |> incrementInt"
        result `shouldBe` Right (MTPrim mempty MTInt, int 3)

      it "multiple uses of infix with same type" $ do
        result <- eval testStdlib "let isOne a = a == 1; let apply a f = f a; infix |> = apply; 1 |> incrementInt |> isOne"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "multiple uses of infix with different types" $ do
        result <- eval testStdlib "let apply a f = f a; infix |> = apply; 1 |> incrementInt |> pureState"
        result `shouldSatisfy` isRight

      it "addInt 1 2" $ do
        result <- eval testStdlib "addInt 1 2"
        result `shouldBe` Right (MTPrim mempty MTInt, int 3)

      it "infix == = addInt; True" $ do
        result <- eval testStdlib "infix == = addInt; True"
        -- can't overwrite built in infix operators
        result `shouldSatisfy` isLeft

      it "infix +++ = addInt; 1 +++ True" $ do
        result <- eval testStdlib "infix +++ = addInt; 1 +++ True"
        -- function typechecking should still work
        result `shouldSatisfy` isLeft

      it "Greater than 1" $ do
        result <- eval testStdlib "10 > 1"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Greater than 2" $ do
        result <- eval testStdlib "1 > 10"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "Greater than 3" $ do
        result <- eval testStdlib "True < 1"
        result `shouldSatisfy` isLeft

      it "Greater than or equal to 1" $ do
        result <- eval testStdlib "10 >= 1"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Greater than or equal to 2" $ do
        result <- eval testStdlib "10 >= 10"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Greater than or equal to 3" $ do
        result <- eval testStdlib "9 >= 10"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "Less than 1" $ do
        result <- eval testStdlib "1 < 10"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Less than 2" $ do
        result <- eval testStdlib "10 < 1"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

      it "Less than or equal to 1" $ do
        result <- eval testStdlib "1 <= 10"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Less than or equal to 2" $ do
        result <- eval testStdlib "10 <= 10"
        result `shouldBe` Right (MTPrim mempty MTBool, bool True)

      it "Less than or equal to 3" $ do
        result <- eval testStdlib "10 <= 9"
        result `shouldBe` Right (MTPrim mempty MTBool, bool False)

    describe "Real stdlib" $ do
      describe "Big stuff that breaks interpreter" $ do
        it "Parses using a lexeme" $ do
          let expr =
                mconcat
                  [ "let lexeme p = parser.left p parser.space0; ",
                    "let bracketL = lexeme (parser.char \"[\"); ",
                    "parser.run bracketL \"[          \""
                  ]
          result <- eval stdlib expr
          result `shouldSatisfy` isRight
        it "Parses a JSON array" $ do
          let expr =
                mconcat
                  [ "let lexeme p = parser.left p parser.space0; ",
                    "let bracketL = lexeme (parser.char \"[\"); ",
                    "let bracketR = lexeme (parser.char \"]\"); ",
                    "let comma = lexeme (parser.char \",\"); ",
                    "let inner = lexeme (parser.char \"d\"); ",
                    "let bigP = parser.right bracketL (parser.left (parser.sepBy comma inner) bracketR); ",
                    "parser.run bigP \"[d,d,d,d]\""
                  ]
          result <- eval stdlib expr
          result `shouldSatisfy` isRight
