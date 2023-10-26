{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.TypecheckSpec (spec) where

import Control.Monad.State
import Data.Bifunctor
import Data.Either
import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Error.Diagnose as Diag
import Smol.Core
import Smol.Core.Typecheck.FromParsedExpr
import Test.Helpers
import Test.Hspec

evalExpr ::
  Text ->
  Either (TCError Annotation) (ResolvedExpr (Type ResolvedDep Annotation))
evalExpr input = case parseExprAndFormatError input of
  Left e -> error (show e)
  Right expr -> testElaborate expr

getLeft :: (Show a) => Either e a -> e
getLeft (Left e) = e
getLeft (Right a) = error (show a)

testElaborate ::
  (Ord ann, Show ann, Monoid ann) =>
  Expr ParseDep ann ->
  Either (TCError ann) (Expr ResolvedDep (Type ResolvedDep ann))
testElaborate expr =
  case elaborate typecheckEnv (fromParsedExpr expr) of
    Right (typedExpr, _) -> pure typedExpr
    Left e -> Left e

spec :: Spec
spec = do
  describe "TypecheckSpec" $ do
    describe "Parse and typecheck" $ do
      let inputs =
            [ ("True", "True"),
              ("False", "False"),
              ("Unit", "Unit"),
              ("(1,2)", "(1, 2)"),
              ("(\\a -> if a then 1 else 2 : Bool -> Int) True", "Int"),
              ("(\\a -> if a then 1 else 2) True", "1 | 2"),
              ("(\\a -> if a then -1 else 2) True", "-1 | 2"),
              ("(\\a -> \\b -> a) True 1", "True"),
              ("(\\a -> 1 : {dog: Bool} -> 1) { dog: True, cat: 1 }", "1"),
              ("(\\rec -> rec.bool) { bool: True }", "True"),
              ("(\\a -> True : (1 | 2) -> True) 1", "True"),
              ("(\\a -> True : (1 | 2 | 3 | 4 | 5 | 6) -> True) 5", "True"),
              ("(1 : Int) + (2 : Int)", "Int"),
              ("1 + 2", "3"),
              ("-1 + 200", "199"),
              ("200 + -100", "100"),
              ("let f = \\a -> a + 41; let g = f 1 == 42; case g of True -> 1", "1"),
              ("1 == 1", "True"),
              ("6 == 7", "False"),
              ("(1 + 2 + 3 : Int)", "Int"),
              ("(1 + 2 + 3 : Int)", "Int"),
              ("(\"horse\" : String)", "String"),
              ("\"hor\" + \"se\"", "\"horse\""),
              ("let a = if True then \"eg\" else \"og\"; a + \"g\"", "\"egg\" | \"ogg\""),
              ( "(\\pair -> case pair of (a,_) -> a : (Bool, Int) -> Bool) (True, 1)",
                "Bool"
              ),
              ( "(\\pair -> case pair of (True, a) -> a | (False,_) -> 0 : (Bool, Int) -> Int) (True,1)",
                "Int"
              ),
              ( "(case (True, 1) of (True, a) -> a: Int)",
                "Int" -- this should remain total as we know it's always True
              ),
              ( "Just True",
                "Maybe True"
              ),
              ( "(Just True : Maybe True)",
                "Maybe True"
              ),
              ( "(Just : a -> Maybe a)",
                "a -> Maybe a"
              ),
              ( "(That 1 : These a 1)",
                "These a 1"
              ),
              ( "These 1 True",
                "These 1 True"
              ),
              ( "(Left 1 : Either 1 Bool)",
                "Either 1 Bool"
              ),
              ( "(Right True : Either e True)",
                "Either e True"
              ),
              ( "(case Just 1 of Just a -> a | _ -> 0 : Int)",
                "Int"
              ),
              ( "(\\a -> case a of 1 -> 10 | 2 -> 20 : (1 | 2) -> Int) 1",
                "Int"
              ),
              ( "(\\a -> case a of (1,_) -> 10 | (2,_) -> 20 : (1 | 2,Bool) -> Int) (1,False)",
                "Int"
              ),
              ( "(\\a -> a : Maybe a -> Maybe a) (Nothing : Maybe Int)",
                "Maybe Int"
              ),
              ( "(\\a -> a : Maybe a -> Maybe a) (Just 1)",
                "Maybe 1"
              ),
              ( "(\\f -> \\ident -> case ident of Identity a -> Identity (f a) : (a -> b) -> Identity a -> Identity b)",
                "(a -> b) -> Identity a -> Identity b"
              ),
              ( "(\\f -> \\maybe -> case maybe of Just a -> Just (f a) | Nothing -> Nothing : (a -> b) -> Maybe a -> Maybe b)",
                "(a -> b) -> Maybe a -> Maybe b"
              ),
              ( "(\\f -> \\maybe -> case maybe of Just a -> Just (f a) | Nothing -> Nothing : (b -> a) -> Maybe b -> Maybe a)",
                "(b -> a) -> Maybe b -> Maybe a"
              ),
              ( "(case (This 42 : These Int Int) of This a -> a : Int)",
                "Int"
              ),
              ( "let fmap = (\\f -> \\maybe -> case maybe of Just a -> Just (f a) | Nothing -> Nothing : (a -> b) -> Maybe a -> Maybe b); let inc = (\\a -> True : Int -> Bool); fmap inc",
                "Maybe Int -> Maybe Bool"
              ),
              ( "let fmap = (\\f -> \\either -> case either of Right a -> Right (f a) | Left e -> Left e : (a -> b) -> Either e a -> Either e b); let inc = (\\a -> True : Int -> Bool); fmap inc",
                "Either e Int -> Either e Bool"
              ),
              ( "let fmap = (\\f -> \\either -> case either of Right a -> Right (f a) | Left e -> Left e : (a -> b) -> Either e a -> Either e b); fmap",
                "(a -> b) -> Either e a -> Either e b"
              ),
              -- ( "let fmap = (\\f -> \\state -> case state of (State sas) -> State (\\s -> case sas s of (a, s) -> (f a, s)) : (a -> b) -> State s a -> State s b) in fmap",
              -- "(a -> b) -> State s a -> State s b"
              -- ),
              ( "let const = (\\a -> \\b -> a : a -> b -> a); const True 100",
                "True"
              ),
              ( "let id = (\\a -> a : a -> a); id True",
                "True"
              ),
              ( "let id = (\\a -> a : a -> a); (id True, id 1)",
                "(True, 1)"
              ),
              ( "(\\f -> \\maybe -> case maybe of Just a -> Just (f a) | Nothing -> Nothing : (a -> b) -> Maybe a -> Maybe b)",
                "(a -> b) -> Maybe a -> Maybe b"
              ),
              ( "(\\maybeF -> \\maybeA -> case (maybeF, maybeA) of (Just f, Just a) -> Just (f a) | _ -> Nothing : Maybe (a -> b) -> Maybe a -> Maybe b)",
                "Maybe (a -> b) -> Maybe a -> Maybe b"
              ),
              ( "(\\value -> \\default -> case value of Right a -> a | Left _ -> default : Either e a -> a -> a)",
                "Either e a -> a -> a"
              ),
              --              ( "let liftA2 = (\\ap -> \\fmap -> \\f -> \\ma -> \\mb -> ap (fmap f ma) mb : (m (a -> b) -> m a -> m b) -> ((a -> b) -> m a -> m b) -> (a -> b -> c) -> m a -> m b -> m c); let add2 = (\\a -> \\b -> a + b : Int -> Int -> Int); liftA2 add2 (Just 1) (Just 2)",
              --              "Maybe Int"
              --          ),
              ("(\\a -> a + 1) 1", "2"),
              ("0 + 0", "0"),
              ("0 + 1", "1"),
              ("1 + 1", "2"),
              ("\"dog\" + \"log\"", "\"doglog\""),
              ("(\"dog\" : String) + (\"log\" : String)", "String"),
              ("let f = \\a -> a + 1; let g = 100; f 1", "2"),
              ("(\\pair -> case pair of (a,b) -> a + b : (Int,Int) -> Int)", "(Int,Int) -> Int"),
              ("let id = (\\i -> i : i -> i); case (Just 1) of Just a -> Just (id a) | Nothing -> Nothing", "Maybe 1"),
              ("[1,2]", "[ 1 | 2 ]"),
              ("[1,2,3,4]", "[1 | 4 | 2 | 3]"),
              ("[True]", "[True]"),
              ("([1,2,3,4] : [Int])", "[Int]"),
              ("case (\"dog\" : String) of \"log\" -> True | _ -> False", "Bool"),
              ("case ([1,2,3] : [Int]) of [a] -> [a] | [_,...b] -> b", "[Int]"),
              ("case ([1,2]: [Int]) of [a,...] -> a | _ -> 0", "Int"),
              ("let a = if True then 1 else 2; let b = if True then 7 else 9; a + b", "8 | 9 | 10 | 11"),
              ("\\a -> a == True", "Bool -> Bool"),
              ("(\\x -> (x 1, x (False,True))) (\\a -> a)", "(1, (False, True))"), -- look! higher rank types
              ("let f = (\\x -> (x 1, x False) : (a -> a) -> (1, False)); let id = \\a -> a; f id", "(1, False)"), -- they need annotation, but that's ok
              ("\\a -> \\b -> if a then a else b", "Bool -> Bool -> Bool"),
              ("\\a -> case a of (b,c) -> if b then b else c", "(Bool,Bool) -> Bool"),
              ("equals (10 : Int) (11: Int)", "Bool"), -- using Eq Int typeclass instance
              ("let maybeFmap = \\f -> \\maybe -> case maybe of Just a -> Just (f a) | Nothing -> Nothing; let useFmap = (\\fmap -> fmap (\\a -> a + 1 : Int -> Int) : ((a -> b) -> f a -> f b) -> f Int -> f Int); useFmap maybeFmap", "Maybe Int -> Maybe Int")
            ]
      traverse_
        ( \(inputExpr, expectedType) -> it (T.unpack inputExpr <> " :: " <> T.unpack expectedType) $ do
            case (,) <$> first (T.pack . show) (evalExpr inputExpr) <*> parseTypeAndFormatError expectedType of
              Right (te, typ) ->
                let result = typeForComparison (getExprAnnotation te) $> ()
                    expected = fromParsedType (typeForComparison typ $> ())
                 in result `shouldBe` expected
              other -> error (show other)
        )
        inputs

    describe "Expected failures" $ do
      let inputs =
            [ "equals (10 : Int) True" -- the two `a`s do not match
            ]
      traverse_
        ( \inputExpr -> it (T.unpack inputExpr <> " fails typechecking") $ do
            first (T.pack . show) (evalExpr inputExpr) `shouldSatisfy` isLeft
        )
        inputs

    describe "reduceType" $ do
      it "Primitives are no-op" $ do
        let ty = unsafeParseType "Int"
        reduceType ty `shouldBe` ty
      it "Happy datatypes are left happy" $ do
        let ty = unsafeParseType "Maybe 1"
        reduceType ty `shouldBe` ty
      it "Types applied to happy datatypes are resolved" $ do
        let ty = TApp () (TFunc () mempty (TVar () "a") (tyCons "Maybe" [tyVar "a"])) (TPrim () TPInt)
            expected = unsafeParseType "Maybe Int"
        reduceType ty `shouldBe` expected
      it "Types applied to happy datatypes are resolved" $ do
        let ty = TApp () (TFunc () mempty (TUnknown () 1) (tyCons "Maybe" [tyUnknown 1])) (TPrim () TPInt)
            expected = unsafeParseType "Maybe Int"
        reduceType ty `shouldBe` expected

    describe "freshen" $ do
      let emptyState = TCState mempty 0 mempty
      it "No unknowns" $ do
        let input = fromParsedType $ unsafeParseType "Int -> Int"
            expected = input
        evalState (freshen input) emptyState `shouldBe` (expected, [])

      it "A -> A becomes 1 -> 1" $ do
        let input = fromParsedType $ TFunc () mempty (tyVar "A") (tyVar "A")
            expected = TFunc () mempty (tyUnknown 0) (tyUnknown 0)
        evalState (freshen input) emptyState `shouldBe` (expected, [Substitution (SubUnknown 0) (TVar () "A")])

    describe "getApplyReturnType" $ do
      it "Simple function" $ do
        let input = fromParsedType $ unsafeParseType "Int -> Int"
            expected = fromParsedType $ unsafeParseType "Int"
        getApplyReturnType input
          `shouldBe` Right (Just expected)

      it "Nested function" $ do
        let input = fromParsedType $ unsafeParseType "Int -> Int -> Bool"
            expected = fromParsedType $ unsafeParseType "Int -> Bool"
        getApplyReturnType input
          `shouldBe` Right (Just expected)

      it "Nested function with constructors" $ do
        let input = fromParsedType $ unsafeParseType "Int -> Maybe Int -> Maybe Bool"
            expected = fromParsedType $ unsafeParseType "Maybe Int -> Maybe Bool"
        getApplyReturnType input
          `shouldBe` Right (Just expected)

      it "Nested higher-order function with constructors" $ do
        let input = fromParsedType $ unsafeParseType "(a -> b) -> Maybe a -> Maybe b"
            expected = fromParsedType $ unsafeParseType "Maybe a -> Maybe b"
        getApplyReturnType input
          `shouldBe` Right (Just expected)

    describe "getRequiredEnv" $ do
      it "Empty" $ do
        freeVars (unsafeParseTypedExpr "\\a -> 1") `shouldBe` mempty

      it "Empty" $ do
        freeVars (unsafeParseTypedExpr "\\a -> c")
          `shouldBe` S.singleton "c"

      it "New var, behind lambda" $ do
        freeVars (unsafeParseTypedExpr "\\a -> \\b -> b + c")
          `shouldBe` S.singleton "c"

      xit "New vars, inside pattern match" $ do
        freeVars (unsafeParseTypedExpr "\\a -> case (1,2) of (c,d) -> c + d + e")
          `shouldBe` S.singleton "e"

    describe "checkPattern" $ do
      it "Match Right a with Either e a" $ do
        let pat = PConstructor () (LocalDefinition "Right") [PVar () "a"]
            ty = fromParsedType (tyCons "Either" [tyVar "e", tyVar "a"])

        fst <$> runTypecheckM typecheckEnv (checkPattern ty pat)
          `shouldBe` Right
            ( PConstructor
                ty
                (LocalDefinition "Right")
                [PVar (fromParsedType $ tyVar "a") "a"]
            )

      it "Match Right True with Either 1 True" $ do
        let pat = PConstructor () (LocalDefinition "Right") [PLiteral () (PBool True)]
            ty = fromParsedType (tyCons "Either" [tyIntLit [1], tyBoolLit True])

        fst <$> runTypecheckM typecheckEnv (checkPattern ty pat)
          `shouldBe` Right
            ( PConstructor
                ty
                (LocalDefinition "Right")
                [PLiteral (fromParsedType $ tyBoolLit True) (PBool True)]
            )

      it "Match Left e with Either e a" $ do
        let pat = PConstructor () (LocalDefinition "Left") [PVar () "e"]
            ty = fromParsedType (tyCons "Either" [tyVar "e", tyVar "a"])

        fst <$> runTypecheckM typecheckEnv (checkPattern ty pat)
          `shouldBe` Right
            ( PConstructor
                ty
                (LocalDefinition "Left")
                [PVar (fromParsedType $ tyVar "e") "e"]
            )

      it "Match State inner with State s a" $ do
        let pat = PConstructor () (LocalDefinition "State") [PVar () "inner"]
            ty = fromParsedType (tyCons "State" [tyVar "s", tyVar "a"])
            tyExpected = TFunc () mempty (tyVar "s") (tyTuple (tyVar "a") [tyVar "s"])

        fst <$> runTypecheckM typecheckEnv (checkPattern ty pat)
          `shouldBe` Right
            ( PConstructor
                ty
                (LocalDefinition "State")
                [PVar (fromParsedType tyExpected) "inner"]
            )

    describe "expected typechecking failures" $ do
      let inputs =
            [ "(\\a -> if a then 1 else True) True",
              "(\\a -> True : (1 | 2) -> True) 3",
              "(\\pair -> case pair of (a,b,c) -> a + b + c : (Int,Int) -> Int) (1,2)",
              "1 + \"dog\"",
              "(case (False, 1) of (True, a) -> a: Int)",
              "(case Just 1 of These a -> a | _ -> 0 : Int)", -- need to lookup constructor
              "(case Just 1 of Just _ a -> a | _ -> 0 : Int)", -- too many args in pattern
              "(case Just 1 of Just -> 1 | _ -> 0 : Int)", -- not enough args in pattern
              "(\\a -> case a of 1 -> 10 | 2 -> 20 | 3 -> 30 : (1 | 2) -> Int) 1" -- pattern contains something not found in union
              -- "Nothing", -- don't know what 'a' is
              -- "This 1" -- don't know what 'b' is
            ]
      traverse_
        ( \inputExpr -> it (T.unpack inputExpr) $ do
            let result = evalExpr inputExpr
            Diag.printDiagnostic Diag.stdout Diag.WithUnicode (Diag.TabSize 2) Diag.defaultStyle (typeErrorDiagnostic inputExpr (getLeft result))
            result `shouldSatisfy` isLeft
        )
        inputs

    describe "Typecheck" $ do
      it "Infers nat" $ do
        let input = EPrim () (PInt 1)
            expected = tyIntLit [1]
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Int literal becomes Int under annotation" $ do
        let input = EAnn () tyInt (EPrim () (PInt 1))
            expected = tyInt

        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

      it "Int literal becomes Int under annotation" $ do
        let input = EAnn () tyInt (EPrim () (PInt 1))
            expected = tyInt

        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

      it "Infers int" $ do
        let input = EPrim () (PInt (-1))
            expected = tyIntLit [-1]
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Int becomes int under annotation" $ do
        let input = EAnn () tyInt (EPrim () (PInt 1))
            expected = tyInt

        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

      it "Infers bool literal true" $ do
        let input = EPrim () (PBool True)
            expected = tyBoolLit True
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Infers bool literal false" $ do
        let input = EPrim () (PBool False)
            expected = tyBoolLit False
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Knows bool literal is bool when annotated" $ do
        let input = EAnn () tyBool (EPrim () (PBool True))
            expected = tyBool
        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

      it "Infers annotated function" $ do
        let input =
              EAnn
                ()
                (TFunc () mempty tyBool tyBool)
                (ELambda () (identifier "a") (var "a"))
            expected =
              TFunc () mempty tyBool tyBool
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Function does not match annotation" $ do
        let input = EAnn () (TFunc () mempty tyBool tyInt) (ELambda () (identifier "a") (var "a"))
        testElaborate input `shouldSatisfy` isLeft

      it "Literal is not function in annotation" $ do
        let input = EAnn () tyInt (ELambda () (identifier "a") (var "a"))
        testElaborate input `shouldSatisfy` isLeft

      it "If statement with annotation" $ do
        let input = EAnn () tyInt (EIf () (bool True) (int 1) (int 2))
            expected = tyInt
        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

      it "If statement with annotation - incorrect pred type" $ do
        let input = EAnn () tyInt (EIf () (int 1) (int 1) (int 2))
        testElaborate input `shouldSatisfy` isLeft

      it "If statement with annotation - mismatched reply types" $ do
        let input = EAnn () tyBool (EIf () (bool True) (int 1) (int 2))
        testElaborate input `shouldSatisfy` isLeft

      it "Application with annotation on function" $ do
        let input =
              EApp
                ()
                ( EAnn
                    ()
                    (TFunc () mempty tyBool tyBool)
                    (unsafeParseExpr "\\a -> a")
                )
                (bool True)
            expected :: Type dep ()
            expected = tyBool
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Application with annotation" $ do
        let input = EAnn () tyBool (unsafeParseExpr "(\\a -> a) True")
            expected :: Type dep ()
            expected = tyBool
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Application with annotation breaks" $ do
        let input = EAnn () tyInt (unsafeParseExpr "(\\a -> a) True")
        testElaborate input `shouldSatisfy` isLeft

      it "Application with no annotation" $ do
        let input = unsafeParseExpr "(\\a -> a) True"
            expected :: Type dep ()
            expected = tyBoolLit True
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Two arg application with no annotation" $ do
        let input = unsafeParseExpr "(\\a -> \\b -> a) True 1"
            expected :: Type dep ()
            expected = tyBoolLit True
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Function use in if" $ do
        let input =
              EAnn
                ()
                tyInt
                (unsafeParseExpr "if ((\\a -> a) True) then 1 else 2")
            expected :: Type dep ()
            expected = tyInt
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Function use in if no annotation" $ do
        let input = unsafeParseExpr "if ((\\a -> a) True) then 1 else 2"
            expected :: Type dep ()
            expected = tyIntLit [1, 2]
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "If statement combines return types but generalises to Int" $ do
        let input =
              EIf
                ()
                (unsafeParseExpr "(\\a -> a) True")
                (EAnn () tyInt (int 1))
                (int 2)
            expected :: Type dep ()
            expected = tyInt
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Detects annotated tuple" $ do
        let input =
              EAnn
                ()
                (tyTuple tyInt [tyBool, tyInt])
                (tuple (int 1) [bool True, int 2])
            expected :: Type dep ()
            expected = tyTuple tyInt [tyBool, tyInt]
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Infers unannotated tuple" $ do
        let input =
              tuple (int 1) [bool True, int 2]
            expected :: Type dep ()
            expected = tyTuple (tyIntLit [1]) [tyBoolLit True, tyIntLit [2]]
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Detects annotated tuple with wrong values" $ do
        let input =
              EAnn
                ()
                (tyTuple tyInt [tyBool, tyInt])
                (tuple (int 1) [int 3, int 2])
        testElaborate input `shouldSatisfy` isLeft

      it "Detects annotated tuple with different length" $ do
        let input =
              EAnn
                ()
                (tyTuple tyInt [tyBool])
                (tuple (int 1) [bool True, int 2])
        testElaborate input
          `shouldBe` Left
            (TCTupleSizeMismatch 3 (tyTuple tyInt [tyBool]))

      it "Uses polymorphic function with annotation of final value" $ do
        let lambda =
              EAnn
                ()
                (TFunc () mempty (tyVar "A") (tyVar "A"))
                (ELambda () "a" (var "a"))

            input =
              EAnn
                ()
                tyInt
                (EApp () lambda (int 1))

            expected :: Type dep ()
            expected = tyInt

        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

      it "Uses polymorphic function" $ do
        let argInput =
              EAnn
                ()
                (TFunc () mempty (tyVar "A") (tyVar "A"))
                (ELambda () "a" (var "a"))
            input = EApp () argInput (int 1)

            expected :: Type dep ()
            expected = tyIntLit [1]

        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

      it "Uses polymorphic function once" $ do
        let argInput =
              EAnn
                ()
                (TFunc () mempty (tyVar "A") (tyVar "A"))
                (ELambda () "a" (var "a"))
            fnInput =
              ELambda
                ()
                "f"
                (EApp () (var "f") (int 1))
            input = EApp () fnInput argInput
            expected :: Type dep ()
            expected = tyIntLit [1]

        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

      it "Uses polymorphic function twice" $ do
        let argInput =
              EAnn
                ()
                (TFunc () mempty (tyVar "A") (tyVar "A"))
                (ELambda () "a" (var "a"))
            fnInput = unsafeParseExpr "\\f -> (f 1, f True)"
            input = EApp () fnInput argInput
            expected :: Type dep ()
            expected = tyTuple (tyIntLit [1]) [tyBoolLit True]

        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

      it "Succeeds when a function wants a subtype of a value but gets the value" $ do
        let input =
              EApp
                ()
                ( EAnn
                    ()
                    (TFunc () mempty tyBool tyInt)
                    (ELambda () "a" (int 100))
                )
                (EAnn () (tyBoolLit True) (bool True))

        testElaborate input `shouldSatisfy` isRight

      it "Fails when a function wants a value but gets a subtype of the value" $ do
        let input =
              EApp
                ()
                ( EAnn
                    ()
                    (TFunc () mempty (tyBoolLit True) tyInt)
                    (ELambda () "a" (int 100))
                )
                (EAnn () tyBool (bool True))

        testElaborate input `shouldSatisfy` isLeft

      it "Fails when a function wants one the True type but gets the False type" $ do
        let input =
              EApp
                ()
                ( EAnn
                    ()
                    (TFunc () mempty (tyBoolLit True) tyInt)
                    (ELambda () "a" (int 100))
                )
                (bool False)

        testElaborate input `shouldSatisfy` isLeft

      it "Record with literals in" $ do
        let input = ERecord () (M.fromList [("a", bool True), ("b", int 1)])

            expected :: Type dep ()
            expected =
              TRecord
                ()
                ( M.fromList
                    [ ("a", tyBoolLit True),
                      ("b", tyIntLit [1])
                    ]
                )

        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

      it "Pass empty record to function that wants an empty one" $ do
        let input =
              EApp
                ()
                (EAnn () (TFunc () mempty (TRecord () mempty) tyInt) (ELambda () "rec" (int 1)))
                (ERecord () mempty)

        testElaborate input `shouldSatisfy` isRight

      it "Pass bigger record to function that wants an empty one" $ do
        let input =
              EApp
                ()
                ( EAnn
                    ()
                    (TFunc () mempty (TRecord () mempty) tyInt)
                    (ELambda () "rec" (int 1))
                )
                (ERecord () (M.singleton "item" (int 1)))

        testElaborate input `shouldSatisfy` isRight

      it "Pass incorrect record to function" $ do
        let input =
              EApp
                ()
                ( EAnn
                    ()
                    (TFunc () mempty (TRecord () (M.singleton "item" tyBool)) tyInt)
                    (ELambda () "rec" (int 1))
                )
                (ERecord () (M.singleton "item" (int 1)))

        testElaborate input `shouldSatisfy` isLeft

      it "Pass empty record to function that wants items" $ do
        let input =
              EApp
                ()
                ( EAnn
                    ()
                    (TFunc () mempty (TRecord () (M.singleton "item" tyBool)) tyInt)
                    (ELambda () "rec" (int 1))
                )
                (ERecord () mempty)

        testElaborate input `shouldSatisfy` isLeft

      it "Patterns have type of input type" $ do
        let input = unsafeParseExpr "(\\maybe -> case maybe of Just b -> 1 | Nothing -> 0 : Maybe Bool -> Int)"
            expected = TFunc () mempty (TApp () (TConstructor () "Maybe") tyBool) tyInt

        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Infers Just with fresh var" $ do
        let input = unsafeParseExpr "Just"
            expected =
              EConstructor
                ( TFunc () mempty (TUnknown () 0) (TApp () (TConstructor () "Maybe") (TUnknown () 0))
                )
                "Just"
        testElaborate input `shouldBe` Right expected

      it "Infers Nothing with fresh var" $ do
        let input = unsafeParseExpr "Nothing"
            expected =
              fromParsedType
                <$> fromParsedExpr
                  ( EConstructor
                      ( tyCons "Maybe" [tyUnknown 0]
                      )
                      "Nothing"
                  )
        testElaborate input `shouldBe` Right expected

      it "Basic let binding" $ do
        let input = unsafeParseExpr "let a = 1; a"
            expected = ELet (tyIntLit [1]) "a" (EPrim (tyIntLit [1]) (PInt 1)) (EVar (tyIntLit [1]) "a")
        testElaborate input `shouldBe` Right expected

      it "Function knows about it's external deps" $ do
        let input = unsafeParseExpr "(\\a -> a : Int -> Int)"
            tyBA = TFunc () mempty tyInt tyInt
            expected =
              ELambda
                tyBA
                "a"
                (EVar tyInt "a")
        let result = case testElaborate input of
              Right (EAnn _ _ body) -> body
              other -> error (show other)
        result `shouldBe` expected

      it "Function knows about it's external deps" $ do
        let input = unsafeParseExpr "(\\a -> \\b -> a : Int -> Int -> Int)"
            tyBA = TFunc () (M.singleton "a" tyInt) tyInt tyInt
            tyABA = TFunc () mempty tyInt tyBA
            expected =
              ELambda
                tyABA
                "a"
                (ELambda tyBA "b" (EVar tyInt "a"))
        let result = case testElaborate input of
              Right (EAnn _ _ body) -> body
              other -> error (show other)
        result `shouldBe` expected

      it "OK boys 1" $ do
        let input =
              unsafeParseExpr
                "let id = (\\i -> i : i -> i); case (Just 1) of Just a -> Just (id a) | Nothing -> Nothing"
            expected = fromParsedType $ unsafeParseType "Maybe 1"
        getExprAnnotation <$> testElaborate input
          `shouldBe` Right
            expected

      it "id function" $ do
        let input = unsafeParseExpr "let id = (\\a -> a : a -> a); id True"
            expected = fromParsedType $ unsafeParseType "True"
        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

      it "const function" $ do
        let input = unsafeParseExpr "let const = (\\a -> \\b -> a : a -> b -> a); const True 100"
            expected = fromParsedType $ unsafeParseType "True"
        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

      it "Weird boys 0" $ do
        let input = unsafeParseExpr "let fmap = (\\f -> case (Just (1 : Int)) of Just a -> Just (f a) : (Int -> b) -> Maybe b); let id = (\\i -> i : Int -> Int); fmap id"
            expected = fromParsedType $ unsafeParseType "Maybe Int"
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Weird boys 4" $ do
        let input =
              unsafeParseExpr
                "let fmap = (\\f -> \\val -> case val of Just aa -> Just (f aa) | Nothing -> Nothing : (a -> b) -> Maybe a -> Maybe b); let id = (\\i -> i : Int -> Int); fmap id (Just 1000)"
            expected = fromParsedType $ unsafeParseType "Maybe Int"
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Weird boys 5" $ do
        let input =
              unsafeParseExpr
                "let fmap = (\\f -> \\maybe -> case maybe of Just a -> Just (f a) | Nothing -> Nothing : (a -> b) -> Maybe a -> Maybe b); let id = (\\i -> i : c -> c); (fmap id (Just 1) : Maybe 1)"
            expected = fromParsedType $ unsafeParseType "Maybe 1"
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Applying with a polymorphic function as arg" $ do
        let input =
              unsafeParseExpr "let apply = (\\f -> \\a -> f a : (a -> b) -> a -> b); let id = (\\c -> c : zz -> zz); apply id 1"
            expected = fromParsedType $ unsafeParseType "1"
        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected
