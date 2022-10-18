{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Typecheck
  ( spec,
  )
where

import Data.Bifunctor
import Data.Either (isLeft)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.NormaliseTypes
import Language.Mimsa.Typechecker.NumberVars
import Language.Mimsa.Typechecker.Typecheck
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Typechecker
import Test.Codegen.Shared
  ( dtEither,
    dtMaybe,
    dtPair,
    dtThese,
  )
import Test.Hspec
import Test.Utils.Helpers

identity :: Monoid ann => Expr Name ann
identity =
  MyLambda
    mempty
    (Identifier mempty "x")
    (MyVar mempty Nothing "x")

startInference :: Expr Name Annotation -> Either TypeError MonoType -> IO ()
startInference = startInferenceWithDataTypes []

dtToMap :: DataType -> Map (Maybe ModuleName, TypeName) DataType
dtToMap dt@(DataType tn _ _) = M.singleton (Nothing, tn) dt

startInferenceWithDataTypes :: [DataType] -> Expr Name Annotation -> Either TypeError MonoType -> IO ()
startInferenceWithDataTypes dts expr expected = do
  let numberedExpr =
        fromRight $
          addNumbersToStoreExpression
            expr
            mempty
  let env = mempty {getDataTypes = mconcat (dtToMap <$> dts)}
  let elabbed =
        fmap (\(_, _, a, _) -> first fst a)
          . typecheck mempty env
          $ numberedExpr
  normaliseType . getTypeFromAnn <$> elabbed `shouldBe` expected
  case elabbed of
    Right elabExpr -> recoverAnn <$> elabExpr `shouldBe` expr
    _ -> pure () -- can't compare

testInfer :: Expr Name Annotation -> Either TypeError MonoType
testInfer expr = do
  numberedExpr <-
    addNumbersToStoreExpression
      expr
      mempty
  let elabbed =
        fmap (\(_, _, a, _) -> a)
          . typecheck mempty mempty
          $ numberedExpr
  getTypeFromAnn <$> elabbed

spec :: Spec
spec = do
  describe "Typecheck" $ do
    describe "basic cases" $ do
      it "infers int" $ do
        let expr = int 1
        startInference expr (Right (MTPrim mempty MTInt))
      it "infers bool" $ do
        let expr = bool True
        startInference expr (Right (MTPrim mempty MTBool))
      it "infers string" $ do
        let expr = str (StringType "hello")
        startInference expr $ Right (MTPrim mempty MTString)
      it "infers let binding" $ do
        let expr =
              MyLet
                mempty
                (Identifier mempty "x")
                (int 42)
                (bool True)
        startInference expr $ Right (MTPrim mempty MTBool)
      it "infers let binding with usage" $ do
        let expr =
              MyLet
                mempty
                (Identifier mempty "x")
                (int 42)
                (MyVar mempty Nothing "x")
        startInference expr $ Right (MTPrim mempty MTInt)
      it "regressions" $ do
        let expr = unsafeParseExpr' "{ fun: (\\a -> let d = 1 in a) }"
        startInference expr $
          Right
            ( MTRecord
                mempty
                ( M.singleton
                    "fun"
                    ( MTFunction
                        mempty
                        (MTVar mempty (TVUnificationVar 1))
                        (MTVar mempty (TVUnificationVar 1))
                    )
                )
                Nothing
            )

      describe "annotations" $ do
        it "annotation that is ok" $ do
          let expr =
                MyAnnotation mempty (MTPrim mempty MTInt) (int 1)
          startInference expr (Right (MTPrim mempty MTInt))
        it "annotation that is not ok" $ do
          let expr =
                MyAnnotation mempty (MTPrim mempty MTInt) (bool True)
          startInference expr (Left (UnificationError mtBool mtInt))
        it "annotation with function" $
          do
            let expr =
                  MyAnnotation
                    mempty
                    (MTFunction mempty (MTPrim mempty MTBool) (MTPrim mempty MTInt))
                    (MyLambda mempty (Identifier mempty "a") (int 1))
            startInference
              expr
              ( Right
                  (MTFunction mempty (MTPrim mempty MTBool) (MTPrim mempty MTInt))
              )
        it "annotation with nested function makes both params the same type" $ do
          let expr =
                MyAnnotation
                  mempty
                  ( MTFunction
                      mempty
                      (MTVar mempty (TVName "a"))
                      ( MTFunction
                          mempty
                          (MTVar mempty (TVName "a"))
                          (MTPrim mempty MTInt)
                      )
                  )
                  ( MyLambda
                      mempty
                      (Identifier mempty "a")
                      ( MyLambda
                          mempty
                          (Identifier mempty "b")
                          (int 1)
                      )
                  )
          startInference
            expr
            ( Right
                ( MTFunction
                    mempty
                    (unknown 1)
                    ( MTFunction
                        mempty
                        (unknown 1)
                        (MTPrim mempty MTInt)
                    )
                )
            )
        it "Let annotation matches value" $ do
          let expr =
                MyLet
                  mempty
                  (Identifier mempty "a")
                  (MyAnnotation mempty mtString (MyLiteral mempty (MyString "dog")))
                  (MyLiteral mempty (MyBool True))
          startInference expr $
            Right mtBool

        it "Let annotation does not match value" $ do
          let expr =
                MyLet
                  mempty
                  (Identifier mempty "a")
                  (MyAnnotation mempty mtInt (MyLiteral mempty (MyString "dog")))
                  (MyLiteral mempty (MyBool True))
          startInference expr $
            Left (UnificationError mtString mtInt)

        it "Lambda annotation matches makes id monomorphic" $ do
          let expr =
                MyAnnotation
                  mempty
                  (MTFunction mempty mtString mtString)
                  ( MyLambda
                      mempty
                      (Identifier mempty "a")
                      (MyVar mempty Nothing "a")
                  )
          startInference expr $
            Right $
              MTFunction mempty mtString mtString

        it "Lambda annotation does not match lambda body" $ do
          let expr =
                MyAnnotation
                  mempty
                  (MTFunction mempty mtString mtInt)
                  ( MyLambda
                      mempty
                      (Identifier mempty "a")
                      (MyInfix mempty Add (MyVar mempty Nothing "a") (int 1))
                  )
          startInference expr $
            Left (UnificationError mtString mtInt)

        it "Applies concrete value to annotated polymorphic function after let generalisation" $ do
          let expr =
                MyLet
                  mempty
                  (Identifier mempty "f")
                  ( MyAnnotation
                      mempty
                      (MTFunction mempty (MTVar mempty (TVName "a")) (MTVar mempty (TVName "a")))
                      (MyLambda mempty (Identifier mempty "a") (MyVar mempty Nothing "a"))
                  )
                  ( MyApp
                      mempty
                      (MyVar mempty Nothing "f")
                      (bool True)
                  )
          startInference expr $
            Right mtBool
      it "infers let binding with recursion 0" $ do
        let expr =
              MyLet
                mempty
                (Identifier mempty "dec")
                ( MyLambda
                    mempty
                    (Identifier mempty "bool")
                    ( MyIf
                        mempty
                        (MyVar mempty Nothing "bool")
                        (bool True)
                        ( MyApp
                            mempty
                            (MyVar mempty Nothing "dec")
                            (bool False)
                        )
                    )
                )
                (MyVar mempty Nothing "dec")
        startInference expr $ Right (MTFunction mempty (MTPrim mempty MTBool) (MTPrim mempty MTBool))

      it "infers let binding with recursion 1" $ do
        let expr =
              MyLet
                mempty
                (Identifier mempty "dec")
                ( MyLambda
                    mempty
                    (Identifier mempty "bool")
                    ( MyIf
                        mempty
                        (MyVar mempty Nothing "bool")
                        (bool True)
                        ( MyApp
                            mempty
                            (MyVar mempty Nothing "dec")
                            (bool False)
                        )
                    )
                )
                (MyApp mempty (MyVar mempty Nothing "dec") (bool False))
        startInference expr $ Right (MTPrim mempty MTBool)

      it "infers let binding with recursion 2 (flipped if cases)" $ do
        let expr =
              MyLet
                mempty
                (Identifier mempty "dec")
                ( MyLambda
                    mempty
                    (Identifier mempty "bool")
                    ( MyIf
                        mempty
                        (MyVar mempty Nothing "bool")
                        ( MyApp
                            mempty
                            (MyVar mempty Nothing "dec")
                            (bool False)
                        )
                        (bool True)
                    )
                )
                (MyApp mempty (MyVar mempty Nothing "dec") (bool False))
        startInference expr $ Right (MTPrim mempty MTBool)

      it "infers multiple let bindings" $ do
        let expr =
              MyLet
                mempty
                (Identifier mempty "x")
                (bool True)
                ( MyLet
                    mempty
                    (Identifier mempty "y")
                    (int 42)
                    (MyVar mempty Nothing "x")
                )
        startInference expr $ Right (MTPrim mempty MTBool)

      it "infers shadowed let bindings" $ do
        let expr =
              MyLet
                mempty
                (Identifier mempty "x")
                (bool True)
                ( MyLet
                    mempty
                    (Identifier mempty "x")
                    (int 42)
                    (MyVar mempty Nothing "x")
                )
        startInference expr $ Right (MTPrim mempty MTInt)

      it "infers const lambda" $ do
        let expr = MyLambda mempty (Identifier mempty "x") (bool True)
        startInference expr $
          Right (MTFunction mempty (unknown 1) (MTPrim mempty MTBool))

      it "infers identity" $ do
        let expr = identity
        startInference expr $ Right (MTFunction mempty (unknown 1) (unknown 1))

      it "infers const function" $ do
        let expr =
              MyLambda
                mempty
                (Identifier mempty "x")
                (MyLambda mempty (Identifier mempty "y") (MyVar mempty Nothing "x"))
        startInference expr $
          Right
            ( MTFunction
                mempty
                (unknown 1)
                (MTFunction mempty (unknown 2) (unknown 1))
            )

      it "infers const applied with boolean" $ do
        let expr =
              MyApp
                mempty
                ( MyLambda
                    mempty
                    (Identifier mempty "x")
                    (bool True)
                )
                (int 1)
        startInference expr $ Right (MTPrim mempty MTBool)
      it "infers identity with int passed to it" $ do
        let expr =
              MyApp
                mempty
                identity
                (int 1)
        startInference expr $ Right (MTPrim mempty MTInt)
      it "passing int to an if statement in a lambda fails" $ do
        let expr =
              MyApp
                mempty
                ( MyLambda
                    mempty
                    (Identifier mempty "x")
                    (MyIf mempty (MyVar mempty Nothing "x") (int 10) (int 10))
                )
                (int 100)
        startInference expr $
          Left
            ( FunctionArgumentMismatch
                mempty
                (MTPrim mempty MTBool)
                (MTPrim mempty MTInt)
            )
      it "fails occurs check" $ do
        let expr =
              MyLambda
                mempty
                (Identifier mempty "x")
                ( MyApp
                    mempty
                    (MyVar mempty Nothing "x")
                    (MyVar mempty Nothing "x")
                )
        startInference expr $
          Left
            ( FailsOccursCheck
                (tvNum 1)
                ( MTFunction
                    mempty
                    (MTVar mempty (tvNum 1))
                    (MTVar mempty (tvNum 2))
                )
            )
      it "infers pair" $ do
        let expr = MyPair mempty (int 1) (bool True)
        startInference expr $
          Right
            (MTPair mempty (MTPrim mempty MTInt) (MTPrim mempty MTBool))
      it "infers and destructures pair" $ do
        let expr =
              MyLetPattern
                mempty
                ( PPair
                    mempty
                    (PVar mempty "a")
                    (PVar mempty "b")
                )
                (MyPair mempty (int 1) (bool True))
                (MyVar mempty Nothing "a")
        startInference expr $ Right (MTPrim mempty MTInt)
      it "infers destructured pair in a lambda" $ do
        let expr =
              MyLambda
                mempty
                (Identifier mempty "x")
                ( MyLetPattern
                    mempty
                    ( PPair
                        mempty
                        (PVar mempty "a")
                        (PVar mempty "b")
                    )
                    (MyVar mempty Nothing "x")
                    (MyVar mempty Nothing "a")
                )
        startInference expr $
          Right
            (MTFunction mempty (MTPair mempty (unknown 1) (unknown 2)) (unknown 1))
      it "infers empty record" $ do
        let expr =
              MyRecord
                mempty
                mempty
        startInference expr $
          Right
            ( MTRecord mempty mempty Nothing
            )
      it "infers record with two ints in it" $ do
        let expr =
              MyRecord
                mempty
                ( M.fromList
                    [ ("dog", int 1),
                      ("cat", int 2)
                    ]
                )
        startInference expr $
          Right
            ( MTRecord
                mempty
                ( M.fromList
                    [ ("dog", MTPrim mempty MTInt),
                      ("cat", MTPrim mempty MTInt)
                    ]
                )
                Nothing
            )
      it "Infers a record literal from a lambda" $ do
        let expr =
              MyLambda
                mempty
                (Identifier mempty "i")
                ( MyIf
                    mempty
                    ( MyRecordAccess
                        mempty
                        (MyVar mempty Nothing "i")
                        "dog"
                    )
                    (int 1)
                    (int 2)
                )
        startInference expr $
          Right
            ( MTFunction
                mempty
                ( MTRecord
                    mempty
                    ( M.singleton
                        "dog"
                        (MTPrim mempty MTBool)
                    )
                    (Just $ unknown 1)
                )
                (MTPrim mempty MTInt)
            )
      it "Infers partial record from lambda" $ do
        let expr =
              MyLambda
                mempty
                (Identifier mempty "a")
                ( MyInfix
                    mempty
                    Add
                    ( MyRecordAccess
                        mempty
                        (MyVar mempty Nothing "a")
                        "int"
                    )
                    (int 1)
                )
        startInference expr $
          Right
            ( MTFunction
                mempty
                ( MTRecord
                    mempty
                    (M.singleton "int" (MTPrim mempty MTInt))
                    (Just $ unknown 1)
                )
                (MTPrim mempty MTInt)
            )

      it "Uses a polymorphic function twice with conflicting types" $ do
        let expr =
              MyLet
                mempty
                (Identifier mempty "id")
                (MyLambda mempty (Identifier mempty "var") (MyVar mempty Nothing "var"))
                ( MyPair
                    mempty
                    (MyApp mempty (MyVar mempty Nothing "id") (int 1))
                    (MyApp mempty (MyVar mempty Nothing "id") (bool True))
                )
        let expected = Right (MTPair mempty (MTPrim mempty MTInt) (MTPrim mempty MTBool))
        startInference expr expected

      it "Simple let pattern with tuple" $ do
        let expr =
              MyLet
                mempty
                (Identifier mempty "pair")
                (MyPair mempty (int 1) (bool True))
                ( MyLetPattern
                    mempty
                    ( PPair
                        mempty
                        (PVar mempty "a")
                        (PVar mempty "b")
                    )
                    (MyVar mempty Nothing "pair")
                    (MyVar mempty Nothing "a")
                )

        let expected = Right (MTPrim mempty MTInt)
        startInference expr expected

      it "Simplified Tuple destructuring" $ do
        let expr =
              MyLambda
                mempty
                (Identifier mempty "tuple")
                ( MyLetPattern
                    mempty
                    ( PPair
                        mempty
                        (PVar mempty "a")
                        (PVar mempty "b")
                    )
                    (MyVar mempty Nothing "tuple")
                    (MyVar mempty Nothing "a")
                )

        let expected =
              Right
                ( MTFunction
                    mempty
                    (MTPair mempty (unknown 1) (unknown 2))
                    (unknown 1)
                )
        startInference expr expected

      it "Tuple destructuring (pattern match)" $ do
        let expr =
              MyLet
                mempty
                (Identifier mempty "fst")
                ( MyLambda
                    mempty
                    (Identifier mempty "tuple")
                    ( MyPatternMatch
                        mempty
                        (MyVar mempty Nothing "tuple")
                        [ ( PPair
                              mempty
                              (PVar mempty "a")
                              (PVar mempty "b"),
                            MyVar mempty Nothing "a"
                          )
                        ]
                    )
                )
                ( MyLet
                    mempty
                    (Identifier mempty "pair")
                    (MyPair mempty (int 1) (bool True))
                    ( MyApp
                        mempty
                        (MyVar mempty Nothing "fst")
                        (MyVar mempty Nothing "pair")
                    )
                )
        let expected = Right (MTPrim mempty MTInt)
        startInference expr expected

      it "Tuple destructuring" $ do
        let expr =
              MyLet
                mempty
                (Identifier mempty "fst")
                ( MyLambda
                    mempty
                    (Identifier mempty "tuple")
                    ( MyLetPattern
                        mempty
                        ( PPair
                            mempty
                            (PVar mempty "a")
                            (PVar mempty "b")
                        )
                        (MyVar mempty Nothing "tuple")
                        (MyVar mempty Nothing "a")
                    )
                )
                ( MyLet
                    mempty
                    (Identifier mempty "pair")
                    (MyPair mempty (int 1) (bool True))
                    (MyApp mempty (MyVar mempty Nothing "fst") (MyVar mempty Nothing "pair"))
                )
        let expected = Right (MTPrim mempty MTInt)
        startInference expr expected

      it "We can use identity with two different datatypes in one expression" $ do
        let lambda =
              MyLambda
                mempty
                (Identifier mempty "a")
                ( MyIf
                    mempty
                    (MyApp mempty identity (MyVar mempty Nothing "a"))
                    (MyApp mempty identity (int 1))
                    (MyApp mempty identity (int 2))
                )
        let expr = MyApp mempty lambda (bool True)
        startInference lambda $
          Right
            ( MTFunction
                mempty
                (MTPrim mempty MTBool)
                (MTPrim mempty MTInt)
            )
        startInference expr $ Right (MTPrim mempty MTInt)
      it "Conflict RecordRows throw an error" $ do
        let expr =
              MyLambda
                mempty
                (Identifier mempty "a")
                ( MyPair
                    mempty
                    ( MyInfix
                        mempty
                        Add
                        (int 1)
                        (MyRecordAccess mempty (MyVar mempty Nothing "a") "prop")
                    )
                    ( MyInfix
                        mempty
                        StringConcat
                        (str "!")
                        (MyRecordAccess mempty (MyVar mempty Nothing "a") "prop")
                    )
                )
        testInfer expr `shouldSatisfy` isLeft
  describe "Pattern matching" $ do
    it "Returns an EmptyPatternMatch error when no patterns supplied" $ do
      let expr = MyPatternMatch mempty (int 1) mempty
      startInference expr $ Left (PatternMatchErr $ EmptyPatternMatch mempty)
    it "Detects an integer does not match a boolean literal" $ do
      let expr =
            MyPatternMatch
              mempty
              (int 1)
              [ (PLit mempty (MyBool True), int 1),
                (PLit mempty (MyBool False), int 2)
              ]
      testInfer expr `shouldSatisfy` isLeft
    it "Matches a boolean literal" $ do
      let expr =
            MyPatternMatch
              mempty
              (bool True)
              [ (PLit mempty (MyBool True), int 1),
                (PLit mempty (MyBool False), int 2)
              ]
      startInference expr $
        Right (MTPrim mempty MTInt)
    it "Detects patterns don't unify" $ do
      let expr =
            MyPatternMatch
              mempty
              (bool True)
              [ (PLit mempty (MyBool True), int 1),
                (PLit mempty (MyInt 1), int 2)
              ]
      testInfer expr
        `shouldSatisfy` isLeft
    it "Detects output exprs don't unify" $ do
      let expr =
            MyPatternMatch
              mempty
              (bool True)
              [ (PLit mempty (MyBool True), int 1),
                (PLit mempty (MyBool False), bool True)
              ]
      testInfer expr
        `shouldSatisfy` isLeft
    it "Matches a boolean with a variable" $ do
      let expr =
            MyPatternMatch
              mempty
              (int 1)
              [(PVar mempty "dog", bool True)]
      startInference expr $
        Right (MTPrim mempty MTBool)
    it "Matches with a variable and uses that variable" $ do
      let expr =
            MyPatternMatch
              mempty
              (int 1)
              [ ( PVar mempty "dog",
                  MyVar mempty Nothing "dog"
                )
              ]
      startInference expr $
        Right (MTPrim mempty MTInt)
    it "Matches with a wildcard expression" $ do
      let expr =
            MyPatternMatch
              mempty
              (int 1)
              [ ( PWildcard mempty,
                  bool True
                )
              ]
      startInference expr $
        Right (MTPrim mempty MTBool)

    it "Matches pattern match values to branch return types" $ do
      let expr =
            MyLambda
              mempty
              (Identifier mempty "a")
              ( MyPatternMatch
                  mempty
                  (MyVar mempty Nothing "a")
                  [ ( PConstructor mempty Nothing "Just" [PVar mempty "as"],
                      MyVar mempty Nothing "as"
                    ),
                    ( PWildcard mempty,
                      int 100
                    )
                  ]
              )

      startInferenceWithDataTypes [dtMaybe] expr $
        Right
          (MTFunction mempty (dataTypeWithVars mempty Nothing "Maybe" [MTPrim mempty MTInt]) (MTPrim mempty MTInt))

    it "Errors when number of args does not match for Just" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyApp mempty (MyConstructor mempty Nothing "Just") (int 1))
              [ ( PConstructor mempty Nothing "Just" [],
                  bool True
                ),
                ( PConstructor mempty Nothing "Nothing" [],
                  bool False
                ),
                (PConstructor mempty Nothing "Just" [PWildcard mempty], bool False)
              ]

      startInferenceWithDataTypes [dtMaybe] expr $
        Left (PatternMatchErr $ ConstructorArgumentLengthMismatch mempty "Just" 1 0)
    it "Matches wildcard inside datatype" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyApp mempty (MyConstructor mempty Nothing "Just") (int 1))
              [ ( PConstructor mempty Nothing "Just" [PWildcard mempty],
                  bool True
                ),
                ( PConstructor mempty Nothing "Nothing" [],
                  bool False
                )
              ]

      startInferenceWithDataTypes [dtMaybe] expr $
        Right (MTPrim mempty MTBool)
    it "Matches value inside datatype" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyApp mempty (MyConstructor mempty Nothing "Just") (int 1))
              [ ( PConstructor mempty Nothing "Just" [PVar mempty "a"],
                  MyVar mempty Nothing "a"
                ),
                ( PConstructor mempty Nothing "Nothing" [],
                  int 0
                )
              ]

      startInferenceWithDataTypes [dtMaybe] expr $
        Right (MTPrim mempty MTInt)

    it "Matches value inside more complex datatype" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyApp mempty (MyConstructor mempty Nothing "That") (int 1))
              [ ( PConstructor mempty Nothing "This" [PWildcard mempty],
                  int 0
                ),
                ( PConstructor mempty Nothing "That" [PVar mempty "b"],
                  MyVar mempty Nothing "b"
                ),
                ( PConstructor mempty Nothing "These" [PWildcard mempty, PVar mempty "b"],
                  MyVar mempty Nothing "b"
                )
              ]

      startInferenceWithDataTypes [dtThese] expr $
        Right (MTPrim mempty MTInt)

    it "Matches nested datatype" $ do
      let val =
            MyApp
              mempty
              (MyConstructor mempty Nothing "Just")
              ( MyApp mempty (MyConstructor mempty Nothing "Just") (bool True)
              )
      let expr =
            MyPatternMatch
              mempty
              val
              [ ( PConstructor
                    mempty
                    Nothing
                    "Just"
                    [ PConstructor
                        mempty
                        Nothing
                        "Just"
                        [PVar mempty "bool"]
                    ],
                  MyVar mempty Nothing "bool"
                ),
                (PWildcard mempty, bool False)
              ]

      startInferenceWithDataTypes [dtMaybe] expr $
        Right (MTPrim mempty MTBool)

    it "Matches pair" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyPair mempty (int 1) (int 2))
              [ ( PPair
                    mempty
                    (PVar mempty "a")
                    (PVar mempty "b"),
                  MyInfix
                    mempty
                    Add
                    (MyVar mempty Nothing "a")
                    (MyVar mempty Nothing "b")
                )
              ]
      startInference expr $
        Right (MTPrim mempty MTInt)

    it "Infers Left type variable in Either from pattern" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyApp mempty (MyConstructor mempty Nothing "Left") (int 1))
              [ ( PConstructor mempty Nothing "Left" [PVar mempty "e"],
                  MyApp mempty (MyConstructor mempty Nothing "Left") (MyVar mempty Nothing "e")
                ),
                ( PConstructor mempty Nothing "Right" [PLit mempty (MyInt 1)],
                  MyApp mempty (MyConstructor mempty Nothing "Right") (int 1)
                ),
                ( PConstructor mempty Nothing "Right" [PVar mempty "a"],
                  MyApp mempty (MyConstructor mempty Nothing "Right") (MyVar mempty Nothing "a")
                )
              ]

      startInferenceWithDataTypes [dtEither] expr $
        Right
          ( dataTypeWithVars
              mempty
              Nothing
              "Either"
              [ MTPrim mempty MTInt,
                MTPrim mempty MTInt
              ]
          )
    it "Infers Right type variable in Either from pattern" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyApp mempty (MyConstructor mempty Nothing "Right") (bool True))
              [ ( PConstructor mempty Nothing "Left" [PLit mempty (MyInt 1)],
                  MyApp mempty (MyConstructor mempty Nothing "Left") (int 1)
                ),
                ( PConstructor mempty Nothing "Left" [PVar mempty "e"],
                  MyApp mempty (MyConstructor mempty Nothing "Left") (MyVar mempty Nothing "e")
                ),
                ( PConstructor mempty Nothing "Right" [PVar mempty "a"],
                  MyApp mempty (MyConstructor mempty Nothing "Right") (MyVar mempty Nothing "a")
                )
              ]

      startInferenceWithDataTypes [dtEither] expr $
        Right
          ( dataTypeWithVars
              mempty
              Nothing
              "Either"
              [ MTPrim mempty MTInt,
                MTPrim mempty MTBool
              ]
          )

    it "Simpler Either example" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyApp mempty (MyConstructor mempty Nothing "Right") (bool True))
              [ ( PConstructor mempty Nothing "Left" [PWildcard mempty],
                  MyApp mempty (MyConstructor mempty Nothing "Left") (int 1)
                ),
                ( PVar mempty "all",
                  MyVar mempty Nothing "all"
                )
              ]

      startInferenceWithDataTypes [dtEither] expr $
        Right
          ( dataTypeWithVars
              mempty
              Nothing
              "Either"
              [ MTPrim mempty MTInt,
                MTPrim mempty MTBool
              ]
          )
    it "Simpler Either example 2" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyApp mempty (MyConstructor mempty Nothing "Left") (bool True))
              [ ( PConstructor mempty Nothing "Right" [PWildcard mempty],
                  MyApp mempty (MyConstructor mempty Nothing "Right") (int 1)
                ),
                ( PVar mempty "all",
                  MyVar mempty Nothing "all"
                )
              ]

      startInferenceWithDataTypes [dtEither] expr $
        Right
          ( dataTypeWithVars
              mempty
              Nothing
              "Either"
              [ MTPrim mempty MTBool,
                MTPrim mempty MTInt
              ]
          )
    it "Getting types from pair" $ do
      let matchExpr =
            MyApp
              mempty
              ( MyApp
                  mempty
                  (MyConstructor mempty Nothing "Pair")
                  (bool True)
              )
              (int 1)

      let expr =
            MyPatternMatch
              mempty
              matchExpr
              [ ( PConstructor mempty Nothing "Pair" [PVar mempty "a", PVar mempty "b"],
                  MyPair mempty (MyVar mempty Nothing "a") (MyVar mempty Nothing "b")
                )
              ]

      startInferenceWithDataTypes [dtPair] expr $
        Right
          ( MTPair
              mempty
              (MTPrim mempty MTBool)
              ( MTPrim mempty MTInt
              )
          )

    it "Fails when record does not match pattern" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyRecord mempty (M.singleton "dog" (int 1)))
              [ ( PRecord
                    mempty
                    ( M.singleton
                        "log"
                        (PWildcard mempty)
                    ),
                  bool True
                )
              ]
      testInfer expr
        `shouldSatisfy` isLeft
    it "Succeeds when record partially matches pattern" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyRecord mempty (M.fromList [("dog", int 1), ("cat", bool True)]))
              [ ( PRecord
                    mempty
                    ( M.singleton
                        "dog"
                        (PVar mempty "a")
                    ),
                  MyVar mempty Nothing "a"
                )
              ]
      startInference expr $
        Right (MTPrim mempty MTInt)
    it "Succeeds when record entirely matches pattern" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyRecord mempty (M.fromList [("dog", int 1), ("cat", int 2)]))
              [ ( PRecord
                    mempty
                    ( M.fromList
                        [ ( "dog",
                            PVar
                              mempty
                              "a"
                          ),
                          ("cat", PVar mempty "b")
                        ]
                    ),
                  MyInfix
                    mempty
                    Add
                    (MyVar mempty Nothing "a")
                    (MyVar mempty Nothing "b")
                )
              ]
      startInference expr $
        Right (MTPrim mempty MTInt)
    it "Spots a missing pattern" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyConstructor mempty Nothing "Nothing")
              [ ( PConstructor mempty Nothing "Just" [PWildcard mempty],
                  bool False
                )
              ]

      startInferenceWithDataTypes [dtMaybe] expr $
        Left
          ( PatternMatchErr
              (MissingPatterns mempty [PConstructor mempty Nothing "Nothing" mempty])
          )
    it "Does substitutions correctly when pattern matching on a variable from a lambda" $ do
      let expr =
            MyLambda
              mempty
              (Identifier mempty "a")
              ( MyPatternMatch
                  mempty
                  (MyVar mempty Nothing "a")
                  [ (PConstructor mempty Nothing "Just" [PVar mempty "as"], MyVar mempty Nothing "as"),
                    (PConstructor mempty Nothing "Nothing" [], MyLiteral mempty (MyInt 100))
                  ]
              )

          mtMaybeInt = dataTypeWithVars mempty Nothing "Maybe" [mtInt]
      startInferenceWithDataTypes [dtMaybe] expr $
        Right (MTFunction mempty mtMaybeInt mtInt)
    it "Does substitutions correctly when pattern matching on a variable from a lambda with application" $ do
      let fn =
            MyLambda
              mempty
              (Identifier mempty "a")
              ( MyLambda
                  mempty
                  (Identifier mempty "b")
                  ( MyPatternMatch
                      mempty
                      (bool True)
                      [ (PLit mempty (MyBool True), MyVar mempty Nothing "a"),
                        (PWildcard mempty, MyVar mempty Nothing "b")
                      ]
                  )
              )
          expr = MyApp mempty (MyApp mempty fn (MyLiteral mempty (MyInt 1))) (MyLiteral mempty (MyBool True))
      startInference expr $
        Left (FunctionArgumentMismatch mempty (MTPrim mempty MTInt) (MTPrim mempty MTBool))
    it "Does substitutions correctly when pattern matching on a variable inside a constructor from a lambda with application" $ do
      let fn =
            MyLambda
              mempty
              (Identifier mempty "maybeA")
              ( MyLambda
                  mempty
                  (Identifier mempty "b")
                  ( MyPatternMatch
                      mempty
                      (MyVar mempty Nothing "maybeA")
                      [ ( PConstructor mempty Nothing "Just" [PVar mempty "a"],
                          MyVar mempty Nothing "a"
                        ),
                        (PWildcard mempty, MyVar mempty Nothing "b")
                      ]
                  )
              )
          maybeExpr =
            MyApp
              mempty
              (MyConstructor mempty Nothing "Just")
              (MyLiteral mempty (MyInt 1))
          expr =
            MyApp
              mempty
              (MyApp mempty fn maybeExpr)
              ( MyLiteral mempty (MyBool True)
              )

      startInferenceWithDataTypes [dtMaybe] expr $
        Left (FunctionArgumentMismatch mempty (MTPrim mempty MTInt) (MTPrim mempty MTBool))

    it "Spots a redundant pattern" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyConstructor mempty Nothing "Nothing")
              [ ( PConstructor mempty Nothing "Just" [PWildcard mempty],
                  bool False
                ),
                (PConstructor mempty Nothing "Nothing" mempty, bool True),
                (PConstructor mempty Nothing "Nothing" mempty, bool True)
              ]

      startInferenceWithDataTypes [dtMaybe] expr $
        Left
          ( PatternMatchErr
              (RedundantPatterns mempty [PConstructor mempty Nothing "Nothing" mempty])
          )
  describe "Variables as constructors" $ do
    it "Let variable as constructor" $ do
      let expr =
            MyLet
              mempty
              (Identifier mempty "f")
              (MyConstructor mempty Nothing "Just")
              (MyApp mempty (MyVar mempty Nothing "f") (int 1))

      startInferenceWithDataTypes [dtMaybe] expr $
        Right
          ( MTTypeApp
              mempty
              (MTConstructor mempty Nothing "Maybe")
              (MTPrim mempty MTInt)
          )
    it "Typed hole suggestions in scope item" $ do
      let expr =
            MyLet
              mempty
              (Identifier mempty "this")
              (bool True)
              (MyIf mempty (MyTypedHole mempty "what") (int 1) (int 2))
      startInference expr $
        Left
          ( TypedHoles
              ( M.singleton
                  "what"
                  (MTPrim mempty MTBool, S.singleton "this")
              )
          )

    it "No typed hole suggestions in scope item" $ do
      let expr =
            MyLet
              mempty
              (Identifier mempty "this")
              (int 1)
              (MyIf mempty (MyTypedHole mempty "what") (int 1) (int 2))
      startInference expr $
        Left
          ( TypedHoles
              ( M.singleton
                  "what"
                  (MTPrim mempty MTBool, mempty)
              )
          )

    it "Suggests a polymorphic value, specialised to fit" $ do
      let expr =
            MyLambda
              mempty
              (Identifier mempty "this")
              (MyIf mempty (MyTypedHole mempty "what") (int 1) (int 2))
      startInference expr $ Left (TypedHoles (M.singleton "what" (MTPrim mempty MTBool, S.singleton "this")))

    describe "type annotations" $ do
      -- needs type annotations to make this make sense
      xit "Lambda variable as constructor" $ do
        let funcType =
              MTFunction
                mempty
                (MTVar mempty (TVName "f"))
                ( MTTypeApp
                    mempty
                    (MTVar mempty (TVName "f"))
                    (MTPrim mempty MTInt)
                )
        let expr =
              MyAnnotation
                mempty
                funcType
                ( MyLambda
                    mempty
                    (Identifier mempty "f")
                    (MyApp mempty (MyVar mempty Nothing "f") (int 1))
                )

        startInferenceWithDataTypes [dtMaybe] expr $
          Right funcType
      -- needs type annotations
      xit "Lambda variable as constructor (multiple application)" $ do
        let expr =
              MyLambda
                mempty
                (Identifier mempty "f")
                (MyApp mempty (MyApp mempty (MyVar mempty Nothing "f") (int 1)) (bool True))

        startInferenceWithDataTypes [dtMaybe] expr $
          Right
            ( MTFunction
                mempty
                ( MTFunction
                    mempty
                    (MTPrim mempty MTInt)
                    ( MTFunction
                        mempty
                        (MTPrim mempty MTBool)
                        ( MTTypeApp
                            mempty
                            ( MTTypeApp
                                mempty
                                (unknown 2)
                                (MTPrim mempty MTInt)
                            )
                            (MTPrim mempty MTBool)
                        )
                    )
                )
                ( MTTypeApp
                    mempty
                    ( MTTypeApp
                        mempty
                        (unknown 2)
                        (MTPrim mempty MTInt)
                    )
                    (MTPrim mempty MTBool)
                )
            )
