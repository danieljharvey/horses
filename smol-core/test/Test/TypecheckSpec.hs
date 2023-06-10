{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.TypecheckSpec (spec) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Either
import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Error.Diagnose (defaultStyle, printDiagnostic, stdout)
import Smol.Core
import Smol.Core.Typecheck.FromParsedExpr
import Test.BuiltInTypes (builtInTypes)
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

-- simplify type for equality check
-- remove anything that can't be described in a type signature
simplifyType :: Type dep ann -> Type dep ann
simplifyType (TFunc ann _ fn arg) =
  TFunc ann mempty (simplifyType fn) (simplifyType arg)
simplifyType (TArray ann _ as) = TArray ann 0 (simplifyType as)
simplifyType other = mapType simplifyType other

testElaborate ::
  (Ord ann, Show ann, Monoid ann) =>
  Expr ParseDep ann ->
  Either (TCError ann) (Expr ResolvedDep (Type ResolvedDep ann))
testElaborate expr = do
  let env =
        TCEnv
          { tceDataTypes = builtInTypes emptyResolvedDep,
            tceVars = mempty,
            tceGlobals = mempty
          }
  case elaborate env (fromParsedExpr expr) of
    Right typedExpr -> pure typedExpr
    Left e -> Left e

typecheckEnv :: TCEnv ()
typecheckEnv = TCEnv mempty mempty (builtInTypes LocalDefinition)

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
              ("(1 + 2 + 3 : Nat)", "Nat"),
              ("(1 + 2 + 3 : Int)", "Int"),
              ("(\"horse\" : String)", "String"),
              ("\"hor\" + \"se\"", "\"horse\""),
              ("let a = if True then \"eg\" else \"og\"; a + \"g\"", "\"egg\" | \"ogg\""),
              ( "(\\pair -> case pair of (a,_) -> a : (Bool, Nat) -> Bool) (True, 1)",
                "Bool"
              ),
              ( "(\\pair -> case pair of (True, a) -> a | (False,_) -> 0 : (Bool, Nat) -> Nat) (True,1)",
                "Nat"
              ),
              ( "(case (True, 1) of (True, a) -> a: Nat)",
                "Nat" -- this should remain total as we know it's always True
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
              ( "(case Just 1 of Just a -> a | _ -> 0 : Nat)",
                "Nat"
              ),
              ( "(\\a -> case a of 1 -> 10 | 2 -> 20 : (1 | 2) -> Nat) 1",
                "Nat"
              ),
              ( "(\\a -> case a of (1,_) -> 10 | (2,_) -> 20 : (1 | 2,Bool) -> Nat) (1,False)",
                "Nat"
              ),
              ( "(\\a -> a : Maybe a -> Maybe a) (Nothing : Maybe Nat)",
                "Maybe Nat"
              ),
              ( "(\\a -> a : Maybe a -> Maybe a) Just 1",
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
              ( "(case (This 42 : These Nat Nat) of This a -> a : Nat)",
                "Nat"
              ),
              ( "let fmap = (\\f -> \\maybe -> case maybe of Just a -> Just (f a) | Nothing -> Nothing : (a -> b) -> Maybe a -> Maybe b); let inc = (\\a -> True : Nat -> Bool); fmap inc",
                "Maybe Nat -> Maybe Bool"
              ),
              ( "let fmap = (\\f -> \\either -> case either of Right a -> Right (f a) | Left e -> Left e : (a -> b) -> Either e a -> Either e b); let inc = (\\a -> True : Nat -> Bool); fmap inc",
                "Either e Nat -> Either e Bool"
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
              --              ( "let liftA2 = (\\ap -> \\fmap -> \\f -> \\ma -> \\mb -> ap (fmap f ma) mb : (m (a -> b) -> m a -> m b) -> ((a -> b) -> m a -> m b) -> (a -> b -> c) -> m a -> m b -> m c); let add2 = (\\a -> \\b -> a + b : Nat -> Nat -> Nat); liftA2 add2 (Just 1) (Just 2)",
              --              "Maybe Nat"
              --          ),
              ("(\\a -> a + 1) 1", "2"),
              ("0 + 0", "0"),
              ("0 + 1", "1"),
              ("1 + 1", "2"),
              ("\\a -> a + 1", "Nat -> Nat"),
              ("(\\pair -> case pair of (a,b) -> a + b : (Nat,Nat) -> Nat)", "(Nat,Nat) -> Nat"),
              ("let id = (\\i -> i : i -> i); case (Just 1) of Just a -> Just (id a) | Nothing -> Nothing", "Maybe 1"),
              ("[1,2]", "[ 1 | 2 ]"),
              ("[1,2,3,4]", "[1 | 4 | 2 | 3]"),
              ("[True]", "[True]"),
              ("([1,2,3,4] : [Nat])", "[Nat]"),
              ("case (\"dog\" : String) of \"log\" -> True | _ -> False", "Bool"),
              ("case ([1,2,3] : [Nat]) of [a] -> [a] | [_,...b] -> b", "[Nat]"),
              ("case ([1,2]: [Nat]) of [a,...] -> a | _ -> 0", "Nat"),
              ("let a = if True then 1 else 2; let b = if True then 7 else 9; a + b", "8 | 9 | 10 | 11"),
              ("(egg! : 42)", "{ egg: 42 } => 42"),
              ("let val = (\\a -> a + egg! : { egg : Int } => Int -> Int); val", "{ egg: Int } => Int -> Int")
            ]
      traverse_
        ( \(inputExpr, expectedType) -> it (T.unpack inputExpr <> " :: " <> T.unpack expectedType) $ do
            case (,) <$> first (T.pack . show) (evalExpr inputExpr) <*> parseTypeAndFormatError expectedType of
              Right (te, typ) ->
                let result = simplifyType (getExprAnnotation te) $> ()
                    expected = fromParsedType (simplifyType typ $> ())
                 in result `shouldBe` expected
              other -> error (show other)
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
        let ty = TApp () (TFunc () mempty (TVar () "a") (tyCons "Maybe" [tyVar "a"])) (TPrim () TPNat)
            expected = unsafeParseType "Maybe Nat"
        reduceType ty `shouldBe` expected
      it "Types applied to happy datatypes are resolved" $ do
        let ty = TApp () (TFunc () mempty (TUnknown () 1) (tyCons "Maybe" [tyUnknown 1])) (TPrim () TPNat)
            expected = unsafeParseType "Maybe Nat"
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
        let input = fromParsedType $ unsafeParseType "Nat -> Int"
            expected = fromParsedType $ unsafeParseType "Int"
        runExcept (getApplyReturnType input)
          `shouldBe` Right expected

      it "Nested function" $ do
        let input = fromParsedType $ unsafeParseType "Nat -> Int -> Bool"
            expected = fromParsedType $ unsafeParseType "Int -> Bool"
        runExcept (getApplyReturnType input)
          `shouldBe` Right expected

      it "Nested function with constructors" $ do
        let input = fromParsedType $ unsafeParseType "Nat -> Maybe Int -> Maybe Bool"
            expected = fromParsedType $ unsafeParseType "Maybe Int -> Maybe Bool"
        runExcept (getApplyReturnType input)
          `shouldBe` Right expected

      it "Nested higher-order function with constructors" $ do
        let input = fromParsedType $ unsafeParseType "(a -> b) -> Maybe a -> Maybe b"
            expected = fromParsedType $ unsafeParseType "Maybe a -> Maybe b"
        runExcept (getApplyReturnType input)
          `shouldBe` Right expected

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

        fst <$> runReaderT (checkPattern ty pat) typecheckEnv
          `shouldBe` Right
            ( PConstructor
                ty
                (LocalDefinition "Right")
                [PVar (fromParsedType $ tyVar "a") "a"]
            )

      it "Match Right True with Either 1 True" $ do
        let pat = PConstructor () (LocalDefinition "Right") [PLiteral () (PBool True)]
            ty = fromParsedType (tyCons "Either" [tyIntLit [1], tyBoolLit True])

        fst <$> runReaderT (checkPattern ty pat) typecheckEnv
          `shouldBe` Right
            ( PConstructor
                ty
                (LocalDefinition "Right")
                [PLiteral (fromParsedType $ tyBoolLit True) (PBool True)]
            )

      it "Match Left e with Either e a" $ do
        let pat = PConstructor () (LocalDefinition "Left") [PVar () "e"]
            ty = fromParsedType (tyCons "Either" [tyVar "e", tyVar "a"])

        fst <$> runReaderT (checkPattern ty pat) typecheckEnv
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

        fst <$> runReaderT (checkPattern ty pat) typecheckEnv
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
              "(\\pair -> case pair of (a,b,c) -> a + b + c : (Nat,Nat) -> Nat) (1,2)",
              "(case (False, 1) of (True, a) -> a: Nat)",
              "(case Just 1 of These a -> a | _ -> 0 : Nat)", -- need to lookup constructor
              "(case Just 1 of Just _ a -> a | _ -> 0 : Nat)", -- too many args in pattern
              "(case Just 1 of Just -> 1 | _ -> 0 : Nat)", -- not enough args in pattern
              "(\\a -> case a of 1 -> 10 | 2 -> 20 | 3 -> 30 : (1 | 2) -> Nat) 1" -- pattern contains something not found in union
              -- "Nothing", -- don't know what 'a' is
              -- "This 1" -- don't know what 'b' is
            ]
      traverse_
        ( \inputExpr -> it (T.unpack inputExpr) $ do
            let result = evalExpr inputExpr
            printDiagnostic stdout True True 2 defaultStyle (typeErrorDiagnostic inputExpr (getLeft result))
            result `shouldSatisfy` isLeft
        )
        inputs

    describe "Typecheck" $ do
      it "Infers nat" $ do
        let input = EPrim () (PNat 1)
            expected = tyIntLit [1]
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Nat literal becomes Nat under annotation" $ do
        let input = EAnn () tyNat (EPrim () (PNat 1))
            expected = tyNat

        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

      it "Nat literal becomes Int under annotation" $ do
        let input = EAnn () tyInt (EPrim () (PNat 1))
            expected = tyInt

        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

      it "Infers int" $ do
        let input = EPrim () (PInt (-1))
            expected = tyIntLit [-1]
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Nat becomes int under annotation" $ do
        let input = EAnn () tyInt (EPrim () (PNat 1))
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

      it "Context-less global fails" $ do
        let input = EGlobal () "numberOfDogs"

        testElaborate input `shouldSatisfy` isLeft

      it "Global with annotation is OK" $ do
        let input = EAnn () tyInt (EGlobal () "numberOfDogs")

            expected :: Type dep ()
            expected = TGlobals () (M.singleton "numberOfDogs" tyInt) tyInt

        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Global type floats upwards from tuple" $ do
        let input =
              EAnn
                ()
                (tyTuple tyInt [tyBool])
                (tuple (EGlobal () "numberOfDogs") [bool True])

            expected :: Type dep ()
            expected =
              TGlobals
                ()
                (M.singleton "numberOfDogs" tyInt)
                ( tyTuple tyInt [tyBool]
                )

        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Global type floats upwards from lambda" $ do
        let input =
              EApp
                ()
                ( EAnn
                    ()
                    (TFunc () mempty tyInt tyInt)
                    ( ELambda () "a" (EGlobal () "numberOfDogs")
                    )
                )
                (int 1)

            expected :: Type dep ()
            expected =
              TGlobals
                ()
                (M.singleton "numberOfDogs" tyInt)
                tyInt

        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      -- we are losing globals, need an elegant way to collect them
      -- maybe Writer like the Substitutions
      it "Global type floats up from deep" $ do
        let input = unsafeParseExpr "(\\b -> if b then one! + 1 else two! : Bool -> Nat)"
        getExprAnnotation <$> testElaborate input
          `shouldBe` Right
            ( TGlobals
                ()
                (M.fromList [("one", tyNat), ("two", tyNat)])
                (TFunc () mempty tyBool tyNat)
            )

      it "Fails when two global types contradict one another" $ do
        let input = tuple (EAnn () tyInt (EGlobal () "dogs")) [EAnn () tyBool (EGlobal () "dogs")]

        testElaborate input `shouldSatisfy` isLeft

      it "Global doesn't float outside it's let" $ do
        let input =
              EGlobalLet
                ()
                "dog"
                (bool True)
                (EAnn () tyBool (EGlobal () "dog"))

            expected :: Type dep ()
            expected = tyBool

        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Providing a global means we can infer it's type when used" $ do
        let input =
              EGlobalLet
                ()
                "dog"
                (bool True)
                (EGlobal () "dog")

            expected :: Type dep ()
            expected = tyBoolLit True

        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Empty record" $ do
        let input = ERecord () mempty

            expected :: Type dep ()
            expected = TRecord () mempty

        getExprAnnotation <$> testElaborate input
          `shouldBe` Right expected

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
        let input = unsafeParseExpr "(\\maybe -> case maybe of Just b -> 1 | Nothing -> 0 : Maybe Bool -> Nat)"
            expected = TFunc () mempty (TApp () (TConstructor () "Maybe") tyBool) tyNat

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
            expected = ELet (tyIntLit [1]) "a" (EPrim (tyIntLit [1]) (PNat 1)) (EVar (tyIntLit [1]) "a")
        testElaborate input `shouldBe` Right expected

      it "Function knows about it's external deps" $ do
        let input = unsafeParseExpr "(\\a -> a : Nat -> Nat)"
            tyBA = TFunc () mempty tyNat tyNat
            expected =
              ELambda
                tyBA
                "a"
                (EVar tyNat "a")
        let result = case testElaborate input of
              Right (EAnn _ _ body) -> body
              other -> error (show other)
        result `shouldBe` expected

      it "Function knows about it's external deps" $ do
        let input = unsafeParseExpr "(\\a -> \\b -> a : Nat -> Nat -> Nat)"
            tyBA = TFunc () (M.singleton "a" tyNat) tyNat tyNat
            tyABA = TFunc () mempty tyNat tyBA
            expected =
              ELambda
                tyABA
                "a"
                (ELambda tyBA "b" (EVar tyNat "a"))
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
        let input = unsafeParseExpr "let fmap = (\\f -> case (Just (1 : Nat)) of Just a -> Just (f a) : (Nat -> b) -> Maybe b); let id = (\\i -> i : Nat -> Nat); fmap id"
            expected = fromParsedType $ unsafeParseType "Maybe Nat"
        getExprAnnotation <$> testElaborate input `shouldBe` Right expected

      it "Weird boys 4" $ do
        let input =
              unsafeParseExpr
                "let fmap = (\\f -> \\val -> case val of Just aa -> Just (f aa) | Nothing -> Nothing : (a -> b) -> Maybe a -> Maybe b); let id = (\\i -> i : Nat -> Nat); fmap id (Just 1000)"
            expected = fromParsedType $ unsafeParseType "Maybe Nat"
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
