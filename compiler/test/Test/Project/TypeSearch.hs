{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Project.TypeSearch
  ( spec,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Monad as Actions
import qualified Language.Mimsa.Actions.Typecheck as Actions
import Language.Mimsa.Printer
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
  case Actions.run testStdlib Actions.typeMapForProjectSearch of
    Right (_, _, a) -> a
    Left e ->
      error $ "Resolving typemap in TypeSearch tests: " <> T.unpack (prettyPrint e)

idType :: MonoType
idType = MTFunction mempty (unknown 1) (unknown 1)

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
                  (unknown 1)
                  ( MTFunction
                      mempty
                      (unknown 2)
                      (MTVar mempty (TVUnificationVar 1))
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
                    (dataTypeWithVars mempty Nothing "Maybe" [typeName "a"])
                    (dataTypeWithVars mempty Nothing "Maybe" [typeName "b"])
                )
        typeSearchFromText typeMap "(a -> b) -> (Maybe a) -> (Maybe b)"
          `shouldBe` Right
            ( M.singleton
                "fmapMaybe"
                (normaliseType fmapMaybe)
            )
