{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Substitutions
  ( spec,
  )
where

import qualified Data.Map.Strict as M
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Types.Typechecker.Substitutions
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec = do
  describe "Substitutions" $ do
    describe "Preserve the original annotations" $ do
      it "Empty is no-op" $ do
        let mt = MTPrim (Location 1 2) MTInt
            subs = mempty
        applySubst subs mt `shouldBe` mt
      it "Single substitution preserves type" $ do
        let mt = MTVar (Location 1 3) (tvNamed "a")
            subs = Substitutions $ M.singleton (tvNamed "a") (MTPrim (Location 100 123) MTString)
            result = applySubst subs mt
        getAnnotationForType result `shouldBe` getAnnotationForType mt
