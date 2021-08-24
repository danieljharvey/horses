{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker.Elaborate
  ( spec,
  )
where

import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

startElaborate ::
  Expr Variable Annotation ->
  Either TypeError (Expr Variable (MonoType, Annotation))
startElaborate = fmap (\(_, _, a) -> a) . elabAndSubst mempty mempty mempty

spec :: Spec
spec = do
  describe "Elaborate" $ do
    describe "basic cases" $ do
      it "infers int" $ do
        let expr = int 1
        let result = startElaborate expr
        result
          `shouldBe` Right
            ( MyLiteral (MTPrim mempty MTInt, mempty) (MyInt 1)
            )
        (fmap . fmap) recoverAnn result `shouldBe` Right expr

{-
      it "infers bool" $ do
        let expr = bool True
        startElaborate expr `shouldBe` Right (MTPrim mempty MTBool)
      it "infers string" $ do
        let expr = str (StringType "hello")
        startElaborate expr `shouldBe` Right (MTPrim mempty MTString)
      it "infers let binding" $ do
        let expr = MyLet mempty (named "x") (int 42) (bool True)
        startElaborate expr `shouldBe` Right (MTPrim mempty MTBool)
      it "infers let binding with usage" $ do
        let expr = MyLet mempty (named "x") (int 42) (MyVar mempty (named "x"))
        startElaborate expr `shouldBe` Right (MTPrim mempty MTInt)
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
        startElaborate expr `shouldBe` Right (MTFunction mempty (MTPrim mempty MTBool) (MTPrim mempty MTBool))

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
        startElaborate expr `shouldBe` Right (MTPrim mempty MTBool)
-}
