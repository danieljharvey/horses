{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker.Elaborate
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
import Test.Hspec
import Test.Utils.Helpers

startInference :: Expr Variable Annotation -> Either TypeError MonoType
startInference = fmap (\(_, _, a) -> a) . inferAndSubst mempty mempty mempty

spec :: Spec
spec = do
  describe "Elaborate" $ do
    describe "basic cases" $ do
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
        startInference expr `shouldBe` Right (MTFunction mempty (MTPrim mempty MTBool) (MTPrim mempty MTBool))

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
