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
  let result =
        fmap (\(_, _, a, _) -> a)
          . typecheck mempty mempty mempty
          $ input
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
        let expr = MyLiteral (Location 1 4) (MyBool True)
            expected = MyLiteral (MTPrim (Location 1 4) MTBool) (MyBool True)
        startElaborate expr expected

      it "infers string" $ do
        let expr =
              MyLiteral
                (Location 1 10)
                (MyString (StringType "hello"))
            expected =
              MyLiteral
                (MTPrim (Location 1 10) MTString)
                ( MyString
                    (StringType "hello")
                )
        startElaborate expr expected

      it "infers let and var" $ do
        let expr =
              MyLet
                (Location 1 2)
                (named "a")
                (MyLiteral (Location 3 4) (MyInt 1))
                (MyVar (Location 5 6) (named "a"))
            expected =
              MyLet
                (MTPrim (Location 1 2) MTInt)
                (named "a")
                (MyLiteral (MTPrim (Location 3 4) MTInt) (MyInt 1))
                (MyVar (MTPrim (Location 5 6) MTInt) (named "a"))
        startElaborate expr expected

      it "infers let binding" $ do
        let expr =
              MyLet
                (Location 1 2)
                (named "x")
                (MyLiteral (Location 3 4) (MyInt 42))
                (MyLiteral (Location 5 6) (MyBool True))
            expected =
              MyLet
                (MTPrim (Location 1 2) MTBool)
                (named "x")
                (MyLiteral (MTPrim (Location 3 4) MTInt) (MyInt 42))
                (MyLiteral (MTPrim (Location 5 6) MTBool) (MyBool True))
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
                    (Identifier mempty $ named "bool")
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
                    (Identifier mtBool $ named "bool")
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
                    (Identifier mempty $ named "bool")
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
                    (Identifier mtBool $ named "bool")
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
