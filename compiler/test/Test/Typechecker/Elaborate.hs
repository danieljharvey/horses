{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Elaborate
  ( spec,
  )
where

import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.Typecheck
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

startElaborate ::
  Expr Variable Annotation ->
  Expr Variable MonoType ->
  IO ()
startElaborate input expected = do
  let result = fmap (\(_, _, a, _) -> a) . typecheck mempty mempty mempty $ input
  (fmap . fmap) recoverAnn result `shouldBe` Right input
  result `shouldBe` Right expected

mtBool :: MonoType
mtBool = MTPrim mempty MTBool

spec :: Spec
spec = do
  describe "Elaborate" $ do
    describe "basic cases" $ do
      it "infers int" $ do
        let expr = int 1
            expected = MyLiteral (MTPrim mempty MTInt) (MyInt 1)
        startElaborate expr expected

      it "infers bool" $ do
        let expr = bool True
            expected = MyLiteral (MTPrim mempty MTBool) (MyBool True)
        startElaborate expr expected

      it "infers string" $ do
        let expr = str (StringType "hello")
            expected =
              MyLiteral
                (MTPrim mempty MTString)
                ( MyString
                    (StringType "hello")
                )
        startElaborate expr expected

      it "infers let binding" $ do
        let expr = MyLet mempty (named "x") (int 42) (bool True)
            expected =
              MyLet
                (MTPrim mempty MTBool)
                (named "x")
                (MyLiteral (MTPrim mempty MTInt) (MyInt 42))
                (MyLiteral (MTPrim mempty MTBool) (MyBool True))
        startElaborate expr expected

      it "infers let binding with usage" $ do
        let expr = MyLet mempty (named "x") (int 42) (MyVar mempty (named "x"))
            expected =
              MyLet
                (MTPrim mempty MTInt)
                (named "x")
                (MyLiteral (MTPrim mempty MTInt) (MyInt 42))
                ( MyVar (MTPrim mempty MTInt) (named "x")
                )
        startElaborate expr expected

      it "infers let binding with recursion 0" $ do
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
                (MyVar mempty (named "dec"))
            expected =
              MyLet
                (MTFunction mempty mtBool mtBool)
                (named "dec")
                ( MyLambda
                    (MTFunction mempty mtBool mtBool)
                    (named "bool")
                    ( MyIf
                        mtBool
                        (MyVar mtBool (named "bool"))
                        (MyLiteral mtBool (MyBool True))
                        ( MyApp
                            mtBool
                            ( MyVar
                                (MTFunction mempty mtBool mtBool)
                                (named "dec")
                            )
                            (MyLiteral mtBool (MyBool False))
                        )
                    )
                )
                (MyVar (MTFunction mempty mtBool mtBool) (named "dec"))
        startElaborate expr expected

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
            expected =
              MyLet
                mtBool
                (named "dec")
                ( MyLambda
                    (MTFunction mempty mtBool mtBool)
                    (named "bool")
                    ( MyIf
                        mtBool
                        (MyVar mtBool (named "bool"))
                        (MyLiteral mtBool (MyBool True))
                        ( MyApp
                            mtBool
                            (MyVar (MTFunction mempty mtBool mtBool) (named "dec"))
                            (MyLiteral mtBool (MyBool False))
                        )
                    )
                )
                ( MyApp
                    mtBool
                    ( MyVar
                        (MTFunction mempty mtBool mtBool)
                        (named "dec")
                    )
                    (MyLiteral mtBool (MyBool False))
                )
        startElaborate expr expected
