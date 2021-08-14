{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker.Infer
  ( spec,
  )
where

import Language.Mimsa.Typechecker.Infer
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

infer' :: Expr Variable Annotation -> Either TypeError ([Constraint], MonoType)
infer' expr = do
  (constraints, _, tyExpr) <-
    runInferM mempty defaultTcState (infer mempty expr)
  pure (constraints, tyExpr)

spec :: Spec
spec = do
  xdescribe "Infer" $ do
    it "infers int" $ do
      let expr = int 1
      infer' expr `shouldBe` Right ([], MTPrim mempty MTInt)
    it "infers bool" $ do
      let expr = bool True
      infer' expr `shouldBe` Right ([], MTPrim mempty MTBool)
    it "infers string" $ do
      let expr = str (StringType "hello")
      infer' expr `shouldBe` Right ([], MTPrim mempty MTString)
    it "infers let binding" $ do
      let expr = MyLet mempty (named "x") (int 42) (bool True)
      infer' expr
        `shouldBe` Right
          ( [ShouldEqual (unknown 0) (MTPrim mempty MTInt)],
            MTPrim mempty MTBool
          )
    it "infers let binding with usage" $ do
      let expr = MyLet mempty (named "x") (int 42) (MyVar mempty (named "x"))
      infer' expr
        `shouldBe` Right
          ( [ShouldEqual (unknown 0) (MTPrim mempty MTInt)],
            MTPrim mempty MTInt
          )
    it "infers identity function" $ do
      let expr = MyLet mempty (named "id") (MyLambda mempty (named "x") (MyVar mempty (named "x"))) (int 1)
      infer' expr
        `shouldBe` Right
          ( [ ShouldEqual
                (unknown 0)
                (MTFunction mempty (unknown 1) (unknown 1))
            ],
            MTPrim mempty MTInt
          )
    it "infers identity function in use" $ do
      let expr =
            MyLet
              mempty
              (named "id")
              ( MyLambda
                  mempty
                  (named "x")
                  ( MyVar mempty (named "x")
                  )
              )
              (MyVar mempty (named "id"))
      infer' expr
        `shouldBe` Right
          ( [ ShouldEqual
                (unknown 2)
                (MTFunction mempty (unknown 1) (unknown 1)),
              ShouldEqual (unknown 0) (MTFunction mempty (unknown 1) (unknown 1))
            ],
            unknown 2
          )
    it "infers identity function being applied" $ do
      let expr =
            MyLet
              mempty
              (named "id")
              ( MyLambda
                  mempty
                  (named "x")
                  ( MyVar mempty (named "x")
                  )
              )
              (MyApp mempty (MyVar mempty (named "id")) (int 1))
      infer' expr
        `shouldBe` Right
          ( [ ShouldEqual
                (unknown 3)
                (MTFunction mempty (unknown 1) (unknown 1)),
              ShouldEqual
                (unknown 3)
                (MTFunction mempty (MTPrim mempty MTInt) (unknown 2)),
              ShouldEqual
                (unknown 0)
                (MTFunction mempty (unknown 1) (unknown 1))
            ],
            unknown 2
          )

    it "infers let binding with recursion" $ do
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
      infer' expr
        `shouldBe` Right
          ( [ ShouldEqual -- "dec" definition
                (unknown 0)
                (MTFunction mempty (MTPrim mempty MTBool) (unknown 2)),
              ShouldEqual -- "dec" lambda input
                (unknown 2)
                (MTPrim mempty MTBool),
              ShouldEqual
                (unknown 1)
                (MTPrim mempty MTBool),
              ShouldEqual
                (unknown 4)
                (MTFunction mempty (unknown 1) (unknown 2)),
              ShouldEqual
                (unknown 4)
                (MTFunction mempty (MTPrim mempty MTBool) (unknown 3)),
              ShouldEqual
                (unknown 0)
                (MTFunction mempty (unknown 1) (unknown 2))
            ],
            unknown 3
          )

    xit "infers multiple let bindings" $ do
      let expr =
            MyLet
              mempty
              (named "x")
              (bool True)
              (MyLet mempty (named "y") (int 42) (MyVar mempty (named "x")))
      infer' expr `shouldBe` Right ([], MTPrim mempty MTBool)
    xit "infers shadowed let bindings" $ do
      let expr =
            MyLet
              mempty
              (named "x")
              (bool True)
              (MyLet mempty (named "x") (int 42) (MyVar mempty (named "x")))
      infer' expr `shouldBe` Right ([], MTPrim mempty MTInt)
    xit "infers const lambda" $ do
      let expr = MyLambda mempty (named "x") (bool True)
      infer' expr
        `shouldBe` Right ([], MTFunction mempty (unknown 1) (MTPrim mempty MTBool))
