{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Bidirect
  ( spec,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Either
import qualified Data.Map as M
import Language.Mimsa.Typechecker.Bidirect
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Codegen.Shared
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

testEnv :: Environment
testEnv = mempty {getDataTypes = M.singleton "Maybe" dtMaybe}

infer' :: ExpSmall Variable Annotation -> Either TypeError MonoType
infer' expr = expAnn <$> runInferM defaultTcState (infer testEnv expr)

inferExpr ::
  ExpSmall Variable Annotation ->
  Either TypeError (ExpSmall Variable MonoType)
inferExpr expr = runInferM defaultTcState (infer testEnv expr)

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
    it "infers let" $ do
      let expr =
            Let
              mempty
              (named "a")
              (Lit mempty (MyInt 1))
              (Var mempty (named "a"))
      infer' expr `shouldBe` Right mtInt
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
        `shouldBe` Right
          ( MTFunction
              mempty
              (MTVar mempty (TVName "a"))
              mtInt
          )
    it "infers application on un-annotated function" $ do
      let lambda =
            Lambda mempty (named "a") (Lit mempty (MyBool True))
          expr = App mempty lambda (Lit mempty (MyInt 100))
      infer' expr `shouldSatisfy` isLeft

    it "infers application on polymorphic function" $ do
      let lambda =
            Ann
              mempty
              ( MTFunction
                  mempty
                  (MTVar mempty (TVNum 1))
                  (MTVar mempty (TVNum 1))
              )
              (Lambda mempty (named "aa") (Var mempty (named "aa")))
          expr =
            Let
              mempty
              (named "id")
              lambda
              ( App
                  mempty
                  (Var mempty (named "id"))
                  (Lit mempty (MyInt 100))
              )
      let result = inferExpr expr
      print result
      expAnn <$> result `shouldBe` Right mtInt

    it "infers application onto annotated function" $ do
      let lambda =
            Ann
              mempty
              (MTFunction mempty mtInt mtBool)
              (Lambda mempty (named "a") (Lit mempty (MyBool True)))
          expr = App mempty lambda (Lit mempty (MyInt 100))
      infer' expr `shouldBe` Right mtBool
    it "if fails when then and else are different" $ do
      let expr =
            If
              mempty
              (Lit mempty (MyBool True))
              (Lit mempty (MyBool False))
              (Lit mempty (MyInt 101))
      infer' expr `shouldSatisfy` isLeft
    it "infers if without annotation" $ do
      let expr =
            If
              mempty
              (Lit mempty (MyBool True))
              (Lit mempty (MyInt 100))
              (Lit mempty (MyInt 101))
      infer' expr `shouldBe` Right mtInt
    it "infers raw constructor" $ do
      let expr = Constructor mempty "Just"
      infer' expr
        `shouldBe` Right
          ( MTFunction
              mempty
              (MTVar mempty (TVNum 0))
              ( MTTypeApp
                  mempty
                  (MTConstructor mempty "Maybe")
                  (MTVar mempty (TVNum 0))
              )
          )
    xit "infers constructor application" $ do
      let expr =
            App
              mempty
              (Constructor mempty "Just")
              (Lit mempty (MyBool True))
      infer' expr
        `shouldBe` Right
          ( MTTypeApp
              mempty
              (MTConstructor mempty "Maybe")
              mtBool
          )
