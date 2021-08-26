{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker.Elaborate
  ( spec,
  )
where

import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

startElaborate ::
  Expr Variable Annotation ->
  Expr Variable (MonoType, Annotation) ->
  IO ()
startElaborate input expected = do
  let result = fmap (\(_, _, a) -> a) . elabAndSubst mempty mempty mempty $ input
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
            expected = MyLiteral (MTPrim mempty MTInt, mempty) (MyInt 1)
        startElaborate expr expected

      it "infers bool" $ do
        let expr = bool True
            expected = MyLiteral (MTPrim mempty MTBool, mempty) (MyBool True)
        startElaborate expr expected

      it "infers string" $ do
        let expr = str (StringType "hello")
            expected = MyLiteral (MTPrim mempty MTString, mempty) (MyString (StringType "hello"))
        startElaborate expr expected

      it "infers let binding" $ do
        let expr = MyLet mempty (named "x") (int 42) (bool True)
            expected =
              MyLet
                (MTPrim mempty MTBool, mempty)
                (named "x")
                (MyLiteral (MTPrim mempty MTInt, mempty) (MyInt 42))
                (MyLiteral (MTPrim mempty MTBool, mempty) (MyBool True))
        startElaborate expr expected

      it "infers let binding with usage" $ do
        let expr = MyLet mempty (named "x") (int 42) (MyVar mempty (named "x"))
            expected =
              MyLet
                (MTPrim mempty MTInt, mempty)
                (named "x")
                (MyLiteral (MTPrim mempty MTInt, mempty) (MyInt 42))
                ( MyVar (MTPrim mempty MTInt, mempty) (named "x")
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
                (MTFunction mempty mtBool mtBool, mempty)
                (named "dec")
                ( MyLambda
                    (MTFunction mempty mtBool mtBool, mempty)
                    (named "bool")
                    ( MyIf
                        (mtBool, mempty)
                        (MyVar (mtBool, mempty) (named "bool"))
                        (MyLiteral (mtBool, mempty) (MyBool True))
                        ( MyApp
                            (mtBool, mempty)
                            ( MyVar
                                (MTFunction mempty mtBool mtBool, mempty)
                                (named "dec")
                            )
                            (MyLiteral (mtBool, mempty) (MyBool False))
                        )
                    )
                )
                (MyVar (MTFunction mempty mtBool mtBool, mempty) (named "dec"))
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
                (mtBool, mempty)
                (named "dec")
                ( MyLambda
                    (MTFunction mempty mtBool mtBool, mempty)
                    (named "bool")
                    ( MyIf
                        (mtBool, mempty)
                        (MyVar (mtBool, mempty) (named "bool"))
                        (MyLiteral (mtBool, mempty) (MyBool True))
                        ( MyApp
                            (mtBool, mempty)
                            (MyVar (MTFunction mempty mtBool mtBool, mempty) (named "dec"))
                            (MyLiteral (mtBool, mempty) (MyBool False))
                        )
                    )
                )
                ( MyApp
                    (mtBool, mempty)
                    ( MyVar
                        (MTFunction mempty mtBool mtBool, mempty)
                        (named "dec")
                    )
                    (MyLiteral (mtBool, mempty) (MyBool False))
                )
        startElaborate expr expected
