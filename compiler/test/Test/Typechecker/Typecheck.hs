{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker.Typecheck
  ( spec,
  )
where

import Data.Either (isLeft)
import qualified Data.Map as M
import Language.Mimsa.Typechecker.Infer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Codegen.Shared
  ( dtEither,
    dtMaybe,
    dtPair,
    dtThese,
  )
import Test.Hspec
import Test.Utils.Helpers

identity :: Monoid ann => Expr Variable ann
identity = MyLambda mempty (named "x") (MyVar mempty (named "x"))

startInference :: Expr Variable Annotation -> Either TypeError MonoType
startInference = fmap snd . inferAndSubst mempty mempty mempty

spec :: Spec
spec = do
  describe "Typecheck" $ do
    fdescribe "basic cases" $ do
      it "infers int" $ do
        let expr = int 1
        startInference expr `shouldBe` Right (MTPrim mempty MTInt)
      it "infers bool" $ do
        let expr = bool True
        startInference expr `shouldBe` Right (MTPrim mempty MTBool)
      it "infers string" $ do
        let expr = str (StringType "hello")
        startInference expr `shouldBe` Right (MTPrim mempty MTString)
      it "infers let binding" $ do
        let expr = MyLet mempty (named "x") (int 42) (bool True)
        startInference expr `shouldBe` Right (MTPrim mempty MTBool)
      it "infers let binding with usage" $ do
        let expr = MyLet mempty (named "x") (int 42) (MyVar mempty (named "x"))
        startInference expr `shouldBe` Right (MTPrim mempty MTInt)
      it "infers let binding with recursion 1" $ do
        let expr =
              MyLet
                mempty
                (named "dec")
                ( MyLambda
                    mempty
                    (named "bool")
                    ( MyIf
                        mempty
                        (MyVar mempty (named "bool"))
                        (bool True)
                        ( MyApp
                            mempty
                            (MyVar mempty (named "dec"))
                            (bool False)
                        )
                    )
                )
                (MyApp mempty (MyVar mempty (named "dec")) (bool False))
        startInference expr `shouldBe` Right (MTPrim mempty MTBool)

      it "infers let binding with recursion 2 (flipped if cases)" $ do
        let expr =
              MyLet
                mempty
                (named "dec")
                ( MyLambda
                    mempty
                    (named "bool")
                    ( MyIf
                        mempty
                        (MyVar mempty (named "bool"))
                        ( MyApp
                            mempty
                            (MyVar mempty (named "dec"))
                            (bool False)
                        )
                        (bool True)
                    )
                )
                (MyApp mempty (MyVar mempty (named "dec")) (bool False))
        startInference expr `shouldBe` Right (MTPrim mempty MTBool)

      it "infers multiple let bindings" $ do
        let expr =
              MyLet
                mempty
                (named "x")
                (bool True)
                (MyLet mempty (named "y") (int 42) (MyVar mempty (named "x")))
        startInference expr `shouldBe` Right (MTPrim mempty MTBool)
      it "infers shadowed let bindings" $ do
        let expr =
              MyLet
                mempty
                (named "x")
                (bool True)
                (MyLet mempty (named "x") (int 42) (MyVar mempty (named "x")))
        startInference expr `shouldBe` Right (MTPrim mempty MTInt)
      it "infers const lambda" $ do
        let expr = MyLambda mempty (named "x") (bool True)
        startInference expr
          `shouldBe` Right (MTFunction mempty (unknown 1) (MTPrim mempty MTBool))
      it "infers identity" $ do
        let expr = identity
        startInference expr `shouldBe` Right (MTFunction mempty (unknown 1) (unknown 1))
      it "infers const function" $ do
        let expr = MyLambda mempty (named "x") (MyLambda mempty (named "y") (MyVar mempty (named "x")))
        startInference expr
          `shouldBe` Right
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
                    (named "x")
                    (bool True)
                )
                (int 1)
        startInference expr `shouldBe` Right (MTPrim mempty MTBool)
      it "infers identity with int passed to it" $ do
        let expr =
              MyApp
                mempty
                identity
                (int 1)
        startInference expr `shouldBe` Right (MTPrim mempty MTInt)
      it "passing int to an if statement in a lambda fails" $ do
        let expr =
              MyApp
                mempty
                ( MyLambda
                    mempty
                    (named "x")
                    (MyIf mempty (MyVar mempty (named "x")) (int 10) (int 10))
                )
                (int 100)
        startInference expr
          `shouldBe` Left
            ( UnificationError (MTPrim mempty MTBool) (MTPrim mempty MTInt)
            )
      it "fails occurs check" $ do
        let expr = MyLambda mempty (named "x") (MyApp mempty (MyVar mempty (named "x")) (MyVar mempty (named "x")))
        startInference expr
          `shouldBe` Left
            ( FailsOccursCheck
                mempty
                (tvFree 0)
                ( MTFunction
                    mempty
                    (MTVar mempty (tvFree 0))
                    (MTVar mempty (tvFree 1))
                )
            )
      it "infers pair" $ do
        let expr = MyPair mempty (int 1) (bool True)
        startInference expr
          `shouldBe` Right
            (MTPair mempty (MTPrim mempty MTInt) (MTPrim mempty MTBool))
      it "infers and destructures pair" $ do
        let expr =
              MyLetPattern
                mempty
                ( PPair
                    mempty
                    (PVar mempty (named "a"))
                    (PVar mempty (named "b"))
                )
                (MyPair mempty (int 1) (bool True))
                (MyVar mempty (named "a"))
        startInference expr `shouldBe` Right (MTPrim mempty MTInt)
      it "infers destructured pair in a lambda" $ do
        let expr =
              MyLambda
                mempty
                (named "x")
                ( MyLetPattern
                    mempty
                    ( PPair
                        mempty
                        (PVar mempty (named "a"))
                        (PVar mempty (named "b"))
                    )
                    (MyVar mempty (named "x"))
                    (MyVar mempty (named "a"))
                )
        startInference expr
          `shouldBe` Right
            (MTFunction mempty (MTPair mempty (unknown 1) (unknown 2)) (unknown 1))
      it "infers empty record" $ do
        let expr =
              MyRecord
                mempty
                mempty
        startInference expr
          `shouldBe` Right
            ( MTRecord mempty mempty
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
        startInference expr
          `shouldBe` Right
            ( MTRecord
                mempty
                ( M.fromList
                    [ ("dog", MTPrim mempty MTInt),
                      ("cat", MTPrim mempty MTInt)
                    ]
                )
            )
      it "Infers a record literal from a lambda" $ do
        let expr =
              MyLambda
                mempty
                (named "i")
                ( MyIf
                    mempty
                    ( MyRecordAccess
                        mempty
                        (MyVar mempty (named "i"))
                        "dog"
                    )
                    (int 1)
                    (int 2)
                )
        startInference expr
          `shouldBe` Right
            ( MTFunction
                mempty
                ( MTRecordRow
                    mempty
                    ( M.singleton
                        "dog"
                        (MTPrim mempty MTBool)
                    )
                    (unknown 1)
                )
                (MTPrim mempty MTInt)
            )
      it "Infers partial record from lambda" $ do
        let expr =
              MyLambda
                mempty
                (named "a")
                ( MyInfix
                    mempty
                    Add
                    ( MyRecordAccess
                        mempty
                        (MyVar mempty (named "a"))
                        "int"
                    )
                    (int 1)
                )
        startInference expr
          `shouldBe` Right
            ( MTFunction
                mempty
                ( MTRecordRow
                    mempty
                    (M.singleton "int" (MTPrim mempty MTInt))
                    (unknown 1)
                )
                (MTPrim mempty MTInt)
            )

      it "Uses a polymorphic function twice with conflicting types" $ do
        let expr =
              MyLet
                mempty
                (named "id")
                (MyLambda mempty (named "var") (MyVar mempty (named "var")))
                ( MyPair
                    mempty
                    (MyApp mempty (MyVar mempty (named "id")) (int 1))
                    (MyApp mempty (MyVar mempty (named "id")) (bool True))
                )
        let expected = Right (MTPair mempty (MTPrim mempty MTInt) (MTPrim mempty MTBool))
        startInference expr `shouldBe` expected
      it "Simple let pattern with tuple" $ do
        let expr =
              MyLet
                mempty
                (named "pair")
                (MyPair mempty (int 1) (bool True))
                ( MyLetPattern
                    mempty
                    ( PPair
                        mempty
                        (PVar mempty (named "a"))
                        (PVar mempty (named "b"))
                    )
                    (MyVar mempty (named "pair"))
                    (MyVar mempty (named "a"))
                )

        let expected = Right (MTPrim mempty MTInt)
        startInference expr `shouldBe` expected

      it "Simplified Tuple destructuring" $ do
        let expr =
              MyLet
                mempty
                (named "fst")
                ( MyLambda
                    mempty
                    (named "tuple")
                    ( MyLetPattern
                        mempty
                        ( PPair
                            mempty
                            (PVar mempty (named "a"))
                            (PVar mempty (named "b"))
                        )
                        (MyVar mempty (named "tuple"))
                        (MyVar mempty (named "a"))
                    )
                )
                (MyVar mempty (named "fst"))

        let expected =
              Right
                ( MTFunction
                    mempty
                    (MTPair mempty (unknown 1) (unknown 2))
                    (unknown 1)
                )
        startInference expr `shouldBe` expected

      it "Tuple destructuring" $ do
        let expr =
              MyLet
                mempty
                (named "fst")
                ( MyLambda
                    mempty
                    (named "tuple")
                    ( MyLetPattern
                        mempty
                        ( PPair
                            mempty
                            (PVar mempty (named "a"))
                            (PVar mempty (named "b"))
                        )
                        (MyVar mempty (named "tuple"))
                        (MyVar mempty (named "a"))
                    )
                )
                ( MyLet
                    mempty
                    (named "pair")
                    (MyPair mempty (int 1) (bool True))
                    (MyApp mempty (MyVar mempty (named "fst")) (MyVar mempty (named "pair")))
                )
        let expected = Right (MTPrim mempty MTInt)
        startInference expr `shouldBe` expected

      it "We can use identity with two different datatypes in one expression" $ do
        let lambda =
              MyLambda
                mempty
                (numbered 100)
                ( MyIf
                    mempty
                    (MyApp mempty identity (MyVar mempty (numbered 100)))
                    (MyApp mempty identity (int 1))
                    (MyApp mempty identity (int 2))
                )
        let expr = MyApp mempty lambda (bool True)
        startInference lambda
          `shouldBe` Right
            ( MTFunction
                mempty
                (MTPrim mempty MTBool)
                (MTPrim mempty MTInt)
            )
        startInference expr `shouldBe` Right (MTPrim mempty MTInt)
      it "Conflict RecordRows throw an error" $ do
        let expr =
              MyLambda
                mempty
                (named "a")
                ( MyPair
                    mempty
                    ( MyInfix
                        mempty
                        Add
                        (int 1)
                        (MyRecordAccess mempty (MyVar mempty (named "a")) "prop")
                    )
                    ( MyInfix
                        mempty
                        StringConcat
                        (str "!")
                        (MyRecordAccess mempty (MyVar mempty (named "a")) "prop")
                    )
                )
        startInference expr `shouldSatisfy` isLeft
  describe "Pattern matching" $ do
    it "Returns an EmptyPatternMatch error when no patterns supplied" $ do
      let expr = MyPatternMatch mempty (int 1) mempty
      startInference expr `shouldBe` Left (PatternMatchErr $ EmptyPatternMatch mempty)
    it "Detects an integer does not match a boolean literal" $ do
      let expr =
            MyPatternMatch
              mempty
              (int 1)
              [ (PLit mempty (MyBool True), int 1),
                (PLit mempty (MyBool False), int 2)
              ]
      startInference expr
        `shouldSatisfy` isLeft
    it "Matches a boolean literal" $ do
      let expr =
            MyPatternMatch
              mempty
              (bool True)
              [ (PLit mempty (MyBool True), int 1),
                (PLit mempty (MyBool False), int 2)
              ]
      startInference expr
        `shouldBe` Right (MTPrim mempty MTInt)
    it "Detects patterns don't unify" $ do
      let expr =
            MyPatternMatch
              mempty
              (bool True)
              [ (PLit mempty (MyBool True), int 1),
                (PLit mempty (MyInt 1), int 2)
              ]
      startInference expr
        `shouldSatisfy` isLeft
    it "Detects output exprs don't unify" $ do
      let expr =
            MyPatternMatch
              mempty
              (bool True)
              [ (PLit mempty (MyBool True), int 1),
                (PLit mempty (MyBool False), bool True)
              ]
      startInference expr
        `shouldSatisfy` isLeft
    it "Matches a boolean with a variable" $ do
      let expr =
            MyPatternMatch
              mempty
              (int 1)
              [(PVar mempty (named "dog"), bool True)]
      startInference expr
        `shouldBe` Right (MTPrim mempty MTBool)
    it "Matches with a variable and uses that variable" $ do
      let expr =
            MyPatternMatch
              mempty
              (int 1)
              [ ( PVar mempty (named "dog"),
                  MyVar mempty (named "dog")
                )
              ]
      startInference expr
        `shouldBe` Right (MTPrim mempty MTInt)
    it "Matches with a wildcard expression" $ do
      let expr =
            MyPatternMatch
              mempty
              (int 1)
              [ ( PWildcard mempty,
                  bool True
                )
              ]
      startInference expr
        `shouldBe` Right (MTPrim mempty MTBool)
    it "An integer does not match with a Maybe" $ do
      let expr =
            MyData
              mempty
              dtMaybe
              ( MyPatternMatch
                  mempty
                  (int 1)
                  [ ( PConstructor mempty "Nothing" [],
                      bool True
                    ),
                    ( PConstructor mempty "Just" [PWildcard mempty],
                      bool False
                    )
                  ]
              )
      startInference expr
        `shouldSatisfy` isLeft
    it "Matches pattern match values to branch return types" $ do
      let expr =
            MyData
              mempty
              dtMaybe
              ( MyLambda
                  mempty
                  (named "a")
                  ( MyPatternMatch
                      mempty
                      (MyVar mempty (named "a"))
                      [ ( PConstructor mempty "Just" [PVar mempty (named "as")],
                          MyVar mempty (named "as")
                        ),
                        ( PWildcard mempty,
                          int 100
                        )
                      ]
                  )
              )
      startInference expr
        `shouldBe` Right
          (MTFunction mempty (MTData mempty "Maybe" [MTPrim mempty MTInt]) (MTPrim mempty MTInt))

    it "Errors when number of args does not match for Just" $ do
      let expr =
            MyData
              mempty
              dtMaybe
              ( MyPatternMatch
                  mempty
                  (MyConsApp mempty (MyConstructor mempty "Just") (int 1))
                  [ ( PConstructor mempty "Just" [],
                      bool True
                    ),
                    ( PConstructor mempty "Nothing" [],
                      bool False
                    ),
                    (PConstructor mempty "Just" [PWildcard mempty], bool False)
                  ]
              )
      startInference expr
        `shouldBe` Left (PatternMatchErr $ ConstructorArgumentLengthMismatch mempty "Just" 1 0)
    it "Matches wildcard inside datatype" $ do
      let expr =
            MyData
              mempty
              dtMaybe
              ( MyPatternMatch
                  mempty
                  (MyConsApp mempty (MyConstructor mempty "Just") (int 1))
                  [ ( PConstructor mempty "Just" [PWildcard mempty],
                      bool True
                    ),
                    ( PConstructor mempty "Nothing" [],
                      bool False
                    )
                  ]
              )
      startInference expr
        `shouldBe` Right (MTPrim mempty MTBool)
    it "Matches value inside datatype" $ do
      let expr =
            MyData
              mempty
              dtMaybe
              ( MyPatternMatch
                  mempty
                  (MyConsApp mempty (MyConstructor mempty "Just") (int 1))
                  [ ( PConstructor mempty "Just" [PVar mempty (named "a")],
                      MyVar mempty (named "a")
                    ),
                    ( PConstructor mempty "Nothing" [],
                      int 0
                    )
                  ]
              )
      startInference expr
        `shouldBe` Right (MTPrim mempty MTInt)
    it "Matches value inside more complex datatype" $ do
      let expr =
            MyData
              mempty
              dtThese
              ( MyPatternMatch
                  mempty
                  (MyConsApp mempty (MyConstructor mempty "That") (int 1))
                  [ ( PConstructor mempty "This" [PWildcard mempty],
                      int 0
                    ),
                    ( PConstructor mempty "That" [PVar mempty (named "b")],
                      MyVar mempty (named "b")
                    ),
                    ( PConstructor mempty "These" [PWildcard mempty, PVar mempty (named "b")],
                      MyVar mempty (named "b")
                    )
                  ]
              )
      startInference expr
        `shouldBe` Right (MTPrim mempty MTInt)
    it "Matches nested datatype" $ do
      let val =
            MyConsApp
              mempty
              (MyConstructor mempty "Just")
              ( MyConsApp mempty (MyConstructor mempty "Just") (bool True)
              )
      let expr =
            MyData
              mempty
              dtMaybe
              ( MyPatternMatch
                  mempty
                  val
                  [ ( PConstructor
                        mempty
                        "Just"
                        [ PConstructor
                            mempty
                            "Just"
                            [PVar mempty (named "bool")]
                        ],
                      MyVar mempty (named "bool")
                    ),
                    (PWildcard mempty, bool False)
                  ]
              )
      startInference expr
        `shouldBe` Right (MTPrim mempty MTBool)
    it "Matches pair" $ do
      let expr =
            MyPatternMatch
              mempty
              (MyPair mempty (int 1) (int 2))
              [ ( PPair
                    mempty
                    (PVar mempty (named "a"))
                    (PVar mempty (named "b")),
                  MyInfix mempty Add (MyVar mempty (named "a")) (MyVar mempty (named "b"))
                )
              ]
      startInference expr
        `shouldBe` Right (MTPrim mempty MTInt)
    it "Infers Left type variable in Either from pattern" $ do
      let expr =
            MyData
              mempty
              dtEither
              ( MyPatternMatch
                  mempty
                  (MyConsApp mempty (MyConstructor mempty "Left") (int 1))
                  [ ( PConstructor mempty "Left" [PVar mempty (named "e")],
                      MyConsApp mempty (MyConstructor mempty "Left") (MyVar mempty (named "e"))
                    ),
                    ( PConstructor mempty "Right" [PLit mempty (MyInt 1)],
                      MyConsApp mempty (MyConstructor mempty "Right") (int 1)
                    ),
                    ( PConstructor mempty "Right" [PVar mempty (named "a")],
                      MyConsApp mempty (MyConstructor mempty "Right") (MyVar mempty (named "a"))
                    )
                  ]
              )
      startInference expr
        `shouldBe` Right
          ( MTData
              mempty
              "Either"
              [ MTPrim mempty MTInt,
                MTPrim mempty MTInt
              ]
          )
    it "Infers Right type variable in Either from pattern" $ do
      let expr =
            MyData
              mempty
              dtEither
              ( MyPatternMatch
                  mempty
                  (MyConsApp mempty (MyConstructor mempty "Right") (bool True))
                  [ ( PConstructor mempty "Left" [PLit mempty (MyInt 1)],
                      MyConsApp mempty (MyConstructor mempty "Left") (int 1)
                    ),
                    ( PConstructor mempty "Left" [PVar mempty (named "e")],
                      MyConsApp mempty (MyConstructor mempty "Left") (MyVar mempty (named "e"))
                    ),
                    ( PConstructor mempty "Right" [PVar mempty (named "a")],
                      MyConsApp mempty (MyConstructor mempty "Right") (MyVar mempty (named "a"))
                    )
                  ]
              )
      startInference expr
        `shouldBe` Right
          ( MTData
              mempty
              "Either"
              [ MTPrim mempty MTInt,
                MTPrim mempty MTBool
              ]
          )
    it "Typechecking pattern matching after lambda" $ do
      let expr =
            MyData
              mempty
              dtMaybe
              ( MyLambda
                  mempty
                  (named "maybe")
                  ( MyPatternMatch
                      mempty
                      (MyVar mempty (named "maybe"))
                      [ ( PConstructor mempty "Just" [PVar mempty (named "a")],
                          MyVar mempty (named "a")
                        ),
                        ( PWildcard mempty,
                          MyVar mempty (named "maybe")
                        )
                      ]
                  )
              )
      startInference expr
        `shouldSatisfy` isLeft

    it "Simpler Either example" $ do
      let expr =
            MyData
              mempty
              dtEither
              ( MyPatternMatch
                  mempty
                  (MyConsApp mempty (MyConstructor mempty "Right") (bool True))
                  [ ( PConstructor mempty "Left" [PWildcard mempty],
                      MyConsApp mempty (MyConstructor mempty "Left") (int 1)
                    ),
                    ( PVar mempty (named "all"),
                      MyVar mempty (named "all")
                    )
                  ]
              )
      startInference expr
        `shouldBe` Right
          ( MTData
              mempty
              "Either"
              [ MTPrim mempty MTInt,
                MTPrim mempty MTBool
              ]
          )
    it "Simpler Either example 2" $ do
      let expr =
            MyData
              mempty
              dtEither
              ( MyPatternMatch
                  mempty
                  (MyConsApp mempty (MyConstructor mempty "Left") (bool True))
                  [ ( PConstructor mempty "Right" [PWildcard mempty],
                      MyConsApp mempty (MyConstructor mempty "Right") (int 1)
                    ),
                    ( PVar mempty (named "all"),
                      MyVar mempty (named "all")
                    )
                  ]
              )
      startInference expr
        `shouldBe` Right
          ( MTData
              mempty
              "Either"
              [ MTPrim mempty MTBool,
                MTPrim mempty MTInt
              ]
          )
    it "Getting types from pair" $ do
      let matchExpr =
            MyConsApp
              mempty
              ( MyConsApp
                  mempty
                  (MyConstructor mempty "Pair")
                  (bool True)
              )
              (int 1)

      let expr =
            MyData
              mempty
              dtPair
              ( MyPatternMatch
                  mempty
                  matchExpr
                  [ ( PConstructor mempty "Pair" [PVar mempty (named "a"), PVar mempty (named "b")],
                      MyPair mempty (MyVar mempty (named "a")) (MyVar mempty (named "b"))
                    )
                  ]
              )
      startInference expr
        `shouldBe` Right
          ( MTPair
              mempty
              (MTPrim mempty MTBool)
              ( MTPrim mempty MTInt
              )
          )
    it "Conflicting types in pair and patterns" $ do
      let matchExpr =
            MyConsApp
              mempty
              ( MyConsApp
                  mempty
                  (MyConstructor mempty "Pair")
                  (bool True)
              )
              (int 1)

      let expr =
            MyData
              mempty
              dtPair
              ( MyPatternMatch
                  mempty
                  matchExpr
                  [ ( PConstructor mempty "Pair" [PLit mempty (MyInt 1), PLit mempty (MyBool True)],
                      MyPair mempty (MyLiteral mempty (MyBool True)) (MyLiteral mempty (MyInt 1))
                    ),
                    ( PConstructor mempty "Pair" [PVar mempty (named "a"), PVar mempty (named "b")],
                      MyPair mempty (MyVar mempty (named "a")) (MyVar mempty (named "b"))
                    )
                  ]
              )
      startInference expr
        `shouldSatisfy` isLeft

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
      startInference expr
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
                        (PVar mempty (named "a"))
                    ),
                  MyVar mempty (named "a")
                )
              ]
      startInference expr
        `shouldBe` Right (MTPrim mempty MTInt)
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
                              (named "a")
                          ),
                          ("cat", PVar mempty (named "b"))
                        ]
                    ),
                  MyInfix
                    mempty
                    Add
                    (MyVar mempty (named "a"))
                    (MyVar mempty (named "b"))
                )
              ]
      startInference expr
        `shouldBe` Right (MTPrim mempty MTInt)
    it "Spots a missing pattern" $ do
      let expr =
            MyData
              mempty
              dtMaybe
              ( MyPatternMatch
                  mempty
                  (MyConstructor mempty "Nothing")
                  [ ( PConstructor mempty "Just" [PWildcard mempty],
                      bool False
                    )
                  ]
              )
      startInference expr
        `shouldBe` Left
          ( PatternMatchErr
              (MissingPatterns mempty [PConstructor mempty "Nothing" mempty])
          )
    it "Does substitutions correctly when pattern matching on a variable from a lambda" $ do
      let expr =
            MyData
              mempty
              dtMaybe
              ( MyLambda
                  mempty
                  (named "a")
                  ( MyPatternMatch
                      mempty
                      (MyVar mempty (named "a"))
                      [ (PConstructor mempty "Just" [PVar mempty (named "as")], MyVar mempty (named "as")),
                        (PConstructor mempty "Nothing" [], MyLiteral mempty (MyInt 100))
                      ]
                  )
              )

          mtInt = MTPrim mempty MTInt
          mtMaybeInt = MTData mempty "Maybe" [mtInt]
      startInference expr
        `shouldBe` Right (MTFunction mempty mtMaybeInt mtInt)
    it "Does substitutions correctly when pattern matching on a variable from a lambda with application" $ do
      let fn =
            MyLambda
              mempty
              (named "a")
              ( MyLambda
                  mempty
                  (named "b")
                  ( MyPatternMatch
                      mempty
                      (bool True)
                      [ (PLit mempty (MyBool True), MyVar mempty (named "a")),
                        (PWildcard mempty, MyVar mempty (named "b"))
                      ]
                  )
              )
          expr = MyApp mempty (MyApp mempty fn (MyLiteral mempty (MyInt 1))) (MyLiteral mempty (MyBool True))
      startInference expr
        `shouldBe` Left (UnificationError (MTPrim mempty MTInt) (MTPrim mempty MTBool))
    it "Does substitutions correctly when pattern matching on a variable inside a constructor from a lambda with application" $ do
      let fn =
            MyLambda
              mempty
              (named "maybeA")
              ( MyLambda
                  mempty
                  (named "b")
                  ( MyPatternMatch
                      mempty
                      (MyVar mempty (named "maybeA"))
                      [ ( PConstructor mempty "Just" [PVar mempty (named "a")],
                          MyVar mempty (named "a")
                        ),
                        (PWildcard mempty, MyVar mempty (named "b"))
                      ]
                  )
              )
          maybeExpr =
            MyConsApp
              mempty
              (MyConstructor mempty "Just")
              (MyLiteral mempty (MyInt 1))
          expr =
            MyData
              mempty
              dtMaybe
              ( MyApp
                  mempty
                  (MyApp mempty fn maybeExpr)
                  ( MyLiteral mempty (MyBool True)
                  )
              )
      startInference expr
        `shouldBe` Left (UnificationError (MTPrim mempty MTInt) (MTPrim mempty MTBool))

    it "Spots a redundant pattern" $ do
      let expr =
            MyData
              mempty
              dtMaybe
              ( MyPatternMatch
                  mempty
                  (MyConstructor mempty "Nothing")
                  [ ( PConstructor mempty "Just" [PWildcard mempty],
                      bool False
                    ),
                    (PConstructor mempty "Nothing" mempty, bool True),
                    (PConstructor mempty "Nothing" mempty, bool True)
                  ]
              )
      startInference expr
        `shouldBe` Left
          ( PatternMatchErr
              (RedundantPatterns mempty [PConstructor mempty "Nothing" mempty])
          )
