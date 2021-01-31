{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Project.TypeSearch
  ( spec,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Actions
import Language.Mimsa.Project.TypeSearch
import Language.Mimsa.Typechecker.NormaliseTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Data.Project (stdLib)
import Test.Hspec
import Test.Utils.Helpers

typeMap :: Map Name MonoType
typeMap =
  case getTypeMap stdLib of
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
        let result = typeSearch typeMap idType
        result
          `shouldBe` M.singleton "id" idType
      it "Finds the addInt and subtractInt functions in test project" $ do
        let addIntType =
              MTFunction
                mempty
                (MTPrim mempty MTInt)
                ( MTFunction
                    mempty
                    (MTPrim mempty MTInt)
                    (MTPrim mempty MTInt)
                )
        let result = typeSearch typeMap addIntType
        result
          `shouldBe` M.fromList
            [ ("addInt", addIntType),
              ("subtractInt", addIntType)
            ]
    describe "from text input" $ do
      it "Finds id function" $ do
        typeSearchFromText typeMap "a -> a"
          `shouldBe` Right (M.singleton "id" idType)
      it "Finds fmapOption" $ do
        let fmapOption =
              MTFunction
                mempty
                (MTFunction mempty (typeName "a") (typeName "b"))
                ( MTFunction
                    mempty
                    (MTData mempty (mkTyCon "Option") [typeName "a"])
                    (MTData mempty (mkTyCon "Option") [typeName "b"])
                )
        typeSearchFromText typeMap "(a -> b) -> (Option a) -> (Option b)"
          `shouldBe` Right
            ( M.singleton
                "fmapOption"
                (normaliseType fmapOption)
            )
