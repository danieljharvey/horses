{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Instantiate
  ( spec,
  )
where

import Language.Mimsa.Typechecker.Instantiate
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec

doInstantiate :: MonoType -> Either TypeError MonoType
doInstantiate mt = runTcMonad mempty (instantiate mt)

spec :: Spec
spec = do
  describe "Instantiate" $ do
    it "If no free vars, it's a no-op" $ do
      doInstantiate MTInt `shouldBe` Right MTInt
    it "If there are vars, but they're free vars, it's a no-op" $ do
      let mt = MTVar (tvFree 1)
      doInstantiate mt `shouldBe` Right mt
    it "If there are bound vars, replace them with free vars" $ do
      let mt = MTFunction (MTVar (tvBound 1)) (MTVar (tvBound 1))
          expected = MTFunction (MTVar (tvFree 1)) (MTVar (tvFree 1))
      doInstantiate mt `shouldBe` Right expected
    it "Replaces multiple bound vars" $ do
      let mt =
            MTFunction
              (MTVar (tvBound 1))
              (MTFunction (MTVar (tvBound 2)) (MTVar (tvBound 1)))
      let expected =
            MTFunction
              (MTVar (tvFree 1))
              (MTFunction (MTVar (tvFree 2)) (MTVar (tvFree 1)))
      doInstantiate mt `shouldBe` Right expected
