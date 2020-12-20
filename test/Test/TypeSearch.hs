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
import Language.Mimsa.Types.Store
import Language.Mimsa.Types.Typechecker
import Test.Data.Project (stdLib)
import Test.Hspec
import Test.Utils.Helpers

stdLibDeps :: ResolvedDeps Annotation
stdLibDeps = case resolveDeps (store stdLib) (getCurrentBindings $ bindings stdLib) of
  Right a -> a
  _ -> error "Error resolving test project"

idType :: MonoType
idType = MTFunction mempty (unknown 1) (unknown 1)

spec :: Spec
spec =
  describe "Typesearch" $ do
    describe "from MonoType" $ do
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
        let result = typeSearch stdLibDeps idType
        result
          `shouldBe` M.singleton (mkName "id") idType
      it "Finds the addInt function in test project" $ do
        let addIntType =
              MTFunction
                mempty
                (MTPrim mempty MTInt)
                ( MTFunction
                    mempty
                    (MTPrim mempty MTInt)
                    (MTPrim mempty MTInt)
                )
        let result = typeSearch stdLibDeps addIntType
        result
          `shouldBe` M.singleton (mkName "addInt") addIntType
    describe "from user input" $ do
      xit "Finds id function" $ do
        let result = typeSearchFromInput stdLibDeps "a -> a"
        result `shouldBe` Right (M.singleton (mkName "id") idType)
