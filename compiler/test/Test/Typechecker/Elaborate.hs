{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Elaborate
  ( spec,
  )
where

import Data.Bifunctor
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.NumberVars
import Language.Mimsa.Typechecker.Typecheck
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

startElaborate ::
  StoreExpression Annotation ->
  Expr Name MonoType ->
  IO ()
startElaborate storeExpr expected = do
  let numberedExpr = fromRight (addNumbers storeExpr)
  let result =
        fmap (\(_, _, a, _) -> first fst a)
          . typecheck mempty mempty
          $ numberedExpr
  (fmap . fmap) recoverAnn result `shouldBe` Right (storeExpression storeExpr)
  result `shouldBe` Right expected

spec :: Spec
spec = do
  describe "Elaborate" $ do
    describe "basic cases" $ do
      it "infers int" $ do
        let expr = int 1
            expected = MyLiteral (MTPrim mempty MTInt) (MyInt 1)
        startElaborate (StoreExpression expr mempty mempty) expected

      it "infers bool" $ do
        let expr = MyLiteral (Location 1 4) (MyBool True)
            expected = MyLiteral (MTPrim (Location 1 4) MTBool) (MyBool True)
        startElaborate (StoreExpression expr mempty mempty) expected

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
        startElaborate (StoreExpression expr mempty mempty) expected

      it "infers let and var" $ do
        let expr =
              MyLet
                (Location 1 2)
                (Identifier (Location 7 8) "a")
                (MyLiteral (Location 3 4) (MyInt 1))
                (MyVar (Location 5 6) "a")
            expected =
              MyLet
                (MTPrim (Location 1 2) MTInt)
                (Identifier (MTPrim (Location 7 8) MTInt) "a")
                (MyLiteral (MTPrim (Location 3 4) MTInt) (MyInt 1))
                (MyVar (MTPrim (Location 5 6) MTInt) "a")
        startElaborate (StoreExpression expr mempty mempty) expected

      it "infers let binding" $ do
        let expr =
              MyLet
                (Location 1 2)
                (Identifier (Location 7 8) "x")
                (MyLiteral (Location 3 4) (MyInt 42))
                (MyLiteral (Location 5 6) (MyBool True))
            expected =
              MyLet
                (MTPrim (Location 1 2) MTBool)
                (Identifier (MTPrim (Location 7 8) MTInt) "x")
                (MyLiteral (MTPrim (Location 3 4) MTInt) (MyInt 42))
                (MyLiteral (MTPrim (Location 5 6) MTBool) (MyBool True))
        startElaborate (StoreExpression expr mempty mempty) expected

      it "infers let binding with usage" $ do
        let expr =
              MyLet
                mempty
                (Identifier mempty "x")
                (int 42)
                (MyVar mempty "x")
            expected =
              MyLet
                (MTPrim mempty MTInt)
                (Identifier (MTPrim mempty MTInt) "x")
                (MyLiteral (MTPrim mempty MTInt) (MyInt 42))
                ( MyVar (MTPrim mempty MTInt) "x"
                )
        startElaborate (StoreExpression expr mempty mempty) expected

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
                        (MyVar mempty "bool")
                        (bool True)
                        ( MyApp
                            mempty
                            (MyVar mempty "dec")
                            (bool False)
                        )
                    )
                )
                (MyVar mempty "dec")
            expected =
              MyLet
                (MTFunction mempty mtBool mtBool)
                (Identifier (MTFunction mempty mtBool mtBool) "dec")
                ( MyLambda
                    (MTFunction mempty mtBool mtBool)
                    (Identifier mtBool "bool")
                    ( MyIf
                        mtBool
                        (MyVar mtBool "bool")
                        (MyLiteral mtBool (MyBool True))
                        ( MyApp
                            mtBool
                            ( MyVar
                                (MTFunction mempty mtBool mtBool)
                                "dec"
                            )
                            (MyLiteral mtBool (MyBool False))
                        )
                    )
                )
                (MyVar (MTFunction mempty mtBool mtBool) "dec")
        startElaborate (StoreExpression expr mempty mempty) expected

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
                        (MyVar mempty "bool")
                        (bool True)
                        ( MyApp
                            mempty
                            (MyVar mempty "dec")
                            (bool False)
                        )
                    )
                )
                (MyApp mempty (MyVar mempty "dec") (bool False))
            expected =
              MyLet
                mtBool
                (Identifier (MTFunction mempty mtBool mtBool) "dec")
                ( MyLambda
                    (MTFunction mempty mtBool mtBool)
                    (Identifier mtBool "bool")
                    ( MyIf
                        mtBool
                        (MyVar mtBool "bool")
                        (MyLiteral mtBool (MyBool True))
                        ( MyApp
                            mtBool
                            (MyVar (MTFunction mempty mtBool mtBool) "dec")
                            (MyLiteral mtBool (MyBool False))
                        )
                    )
                )
                ( MyApp
                    mtBool
                    ( MyVar
                        (MTFunction mempty mtBool mtBool)
                        "dec"
                    )
                    (MyLiteral mtBool (MyBool False))
                )
        startElaborate (StoreExpression expr mempty mempty) expected
