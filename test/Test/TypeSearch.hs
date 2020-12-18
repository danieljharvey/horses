{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.TypeSearch
  ( spec,
  )
where

import qualified Data.Map as M
import Language.Mimsa.Project
import Language.Mimsa.Project.TypeSearch
import Language.Mimsa.Store.ResolvedDeps
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Typechecker
import Test.Data.Project (stdLib)
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec =
  describe "Typesearch" $ do
    it "Equality works despite annotations" $ do
      Normalised (MTPrim mempty MTInt)
        == Normalised (MTPrim (Location 1 2) MTInt)
        `shouldBe` True
    it "Inequality works despite annotations" $ do
      Normalised (MTPrim mempty MTBool)
        == Normalised (MTPrim (Location 1 2) MTInt)
        `shouldBe` False
    it "Finds nothing in an empty project" $ do
      typeSearch mempty (MTPrim mempty MTBool) `shouldBe` mempty
    it "Finds the id function in test project" $ do
      let idType = MTFunction mempty (unknown 1) (unknown 1)
      let deps = resolveDeps (store stdLib) (getCurrentBindings $ bindings stdLib)
      let result = case deps of
            Right deps' -> typeSearch deps' idType
            Left _ -> error "could not resolve deps"
      result
        `shouldBe` M.singleton (mkName "id") idType
