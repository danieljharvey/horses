{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.DataTypes
  ( spec,
  )
where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Codegen.Shared
import Test.Hspec
import Test.Utils.Helpers

runTC ::
  ExceptT TypeError (StateT TypecheckState Identity) a ->
  Either TypeError a
runTC action =
  fst either'
  where
    defaultState =
      TypecheckState 1 mempty
    either' =
      runState
        (runExceptT action)
        defaultState

testInferDataConstructor :: TyCon -> Either TypeError MonoType
testInferDataConstructor tyCon = runTC $ do
  env1 <- storeDataDeclaration mempty mempty dtMaybe
  env2 <- storeDataDeclaration env1 mempty dtEither
  inferDataConstructor env2 mempty tyCon

spec :: Spec
spec = do
  describe "Datatypes" $ do
    it "varsFromDataType" $ do
      varsFromDataType (MTPrim () MTInt) `shouldBe` Nothing
      varsFromDataType (MTConstructor () "Dog") `shouldBe` Just ("Dog", mempty)
      varsFromDataType (MTTypeApp () (MTConstructor () "Dog") (MTPrim () MTInt))
        `shouldBe` Just ("Dog", [MTPrim () MTInt])
      varsFromDataType
        ( MTTypeApp
            ()
            (MTTypeApp () (MTConstructor () "Dog") (MTPrim () MTInt))
            (MTPrim () MTBool)
        )
        `shouldBe` Just ("Dog", [MTPrim () MTInt, MTPrim () MTBool])

    it "Instantiates Maybe" $ do
      testInferDataConstructor "Nothing"
        `shouldBe` Right (MTTypeApp mempty (MTConstructor mempty "Maybe") (unknown 1))
      testInferDataConstructor "Just"
        `shouldBe` Right
          ( MTFunction
              mempty
              (unknown 1)
              ( MTTypeApp mempty (MTConstructor mempty "Maybe") (unknown 1)
              )
          )

    it "Instantiates Either" $ do
      testInferDataConstructor "Left"
        `shouldBe` Right
          ( MTFunction
              mempty
              (unknown 1)
              ( MTTypeApp
                  mempty
                  ( MTTypeApp
                      mempty
                      (MTConstructor mempty "Either")
                      (unknown 1)
                  )
                  (unknown 2)
              )
          )
      testInferDataConstructor "Right"
        `shouldBe` Right
          ( MTFunction
              mempty
              (unknown 2)
              ( MTTypeApp
                  mempty
                  ( MTTypeApp
                      mempty
                      (MTConstructor mempty "Either")
                      (unknown 1)
                  )
                  (unknown 2)
              )
          )
