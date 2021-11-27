{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Project.TypeSearch
  ( spec,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Actions.Shared
import Language.Mimsa.Project.TypeSearch
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.NormaliseTypes
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Data.Project (testStdlib)
import Test.Hspec
import Test.Utils.Helpers

typeMap :: Map Name MonoType
typeMap =
  case getTypeMap testStdlib of
    Right a -> a
    _ -> error "Error resolving test project"

idType :: MonoType
idType = MTFunction mempty (unknown 0) (unknown 0)

spec :: Spec
spec =
  describe "Typesearch" $ do
    describe "from MonoType" $ do
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
              ("subtractInt", addIntType),
              ("int.add", addIntType),
              ("int.subtract", addIntType),
              ( "const",
                MTFunction
                  mempty
                  (unknown 0)
                  ( MTFunction
                      mempty
                      (unknown 1)
                      (MTVar mempty (tvNumbered 0))
                  )
              )
            ]
    describe "from text input" $ do
      it "Finds id function" $ do
        typeSearchFromText typeMap "a -> a"
          `shouldBe` Right (M.singleton "id" (normaliseType idType))
      it "Finds id function from specialised version" $ do
        typeSearchFromText typeMap "String -> String"
          `shouldBe` Right (M.singleton "id" (normaliseType idType))
      it "Finds fmapMaybe" $ do
        let fmapMaybe =
              MTFunction
                mempty
                (MTFunction mempty (typeName "a") (typeName "b"))
                ( MTFunction
                    mempty
                    (dataTypeWithVars mempty "Maybe" [typeName "a"])
                    (dataTypeWithVars mempty "Maybe" [typeName "b"])
                )
        typeSearchFromText typeMap "(a -> b) -> (Maybe a) -> (Maybe b)"
          `shouldBe` Right
            ( M.singleton
                "fmapMaybe"
                (normaliseType fmapMaybe)
            )
