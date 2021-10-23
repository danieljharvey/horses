{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Bidirect
  ( spec,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Either
import Language.Mimsa.Typechecker.Bidirect
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

runInferM ::
  TypecheckState ->
  InferM a ->
  Either TypeError a
runInferM tcState value =
  case either' of
    (Right a, _) -> Right a
    (Left e, _) -> Left e
  where
    either' =
      runState
        (runReaderT (runExceptT value) mempty)
        tcState

infer' :: ExpSmall Variable Annotation -> Either TypeError MonoType
infer' expr = expAnn <$> runInferM defaultTcState (infer mempty expr)

mtInt :: MonoType
mtInt = MTPrim mempty MTInt

mtBool :: MonoType
mtBool = MTPrim mempty MTBool

spec :: Spec
spec = do
  fdescribe "Bidirect" $ do
    it "infers int" $ do
      let expr = Lit mempty (MyInt 1)
      infer' expr `shouldBe` Right mtInt
    {-
    it "infers let" $ do
      let expr = MyLet mempty (named "a") (int 1) (MyVar mempty (named "a"))
      infer' expr `shouldBe` Right (MTPrim mempty MTInt)-}
    it "infers correct annotation" $ do
      let expr = Ann mempty mtInt (Lit mempty (MyInt 1))
      infer' expr `shouldBe` Right mtInt
    it "errors on conflicting annotation" $ do
      let expr = Ann mempty mtBool (Lit mempty (MyInt 1))
      infer' expr `shouldSatisfy` isLeft
    it "infers lambda fails with no annotation" $ do
      let expr =
            Lambda
              mempty
              (named "a")
              (Lit mempty (MyInt 1))
      infer' expr `shouldSatisfy` isLeft
    it "infers lambda succeeds when annotated" $ do
      let expr =
            Ann
              mempty
              (MTFunction mempty (MTVar mempty (TVName "a")) mtInt)
              ( Lambda
                  mempty
                  (named "a")
                  (Lit mempty (MyInt 1))
              )
      infer' expr
        `shouldBe` Right (MTFunction mempty (MTVar mempty (TVName "a")) mtInt)
    it "infers application on un-annotated function" $ do
      let lambda =
            Lambda mempty (named "a") (Lit mempty (MyBool True))
          expr = App mempty lambda (Lit mempty (MyInt 100))
      infer' expr `shouldSatisfy` isLeft
    it "infers application onto annotated function" $ do
      let lambda =
            Ann
              mempty
              (MTFunction mempty mtInt mtBool)
              (Lambda mempty (named "a") (Lit mempty (MyBool True)))
          expr = App mempty lambda (Lit mempty (MyInt 100))
      infer' expr `shouldBe` Right mtBool
