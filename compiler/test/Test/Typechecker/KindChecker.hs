{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.KindChecker (spec) where

import qualified Data.Map as M
import Language.Mimsa.Typechecker.KindChecker
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Codegen.Shared
import Test.Hspec

defaultEnv :: Environment
defaultEnv =
  mempty
    { getDataTypes =
        M.fromList
          [ ("Maybe", dtMaybe),
            ("Either", dtEither),
            ("MatchedPair", dtMatchedPair)
          ]
    }

doKindCheck :: MonoType -> Kind Name
doKindCheck = kindCheck defaultEnv

spec :: Spec
spec = do
  describe "Kind checker" $ do
    it "Maybe is * -> *" $ do
      doKindCheck (MTConstructor mempty "Just")
        `shouldBe` KindArrow KindType KindType
    it "Maybe String is *" $ do
      doKindCheck (MTTypeApp mempty (MTConstructor mempty "Just") (MTPrim mempty MTString))
        `shouldBe` KindType
    it "Either is * -> * -> *" $ do
      doKindCheck
        ( MTConstructor
            mempty
            "Right"
        )
        `shouldBe` KindArrow (KindArrow KindType KindType) KindType
    it "Either String in * -> *" $ do
      doKindCheck
        ( MTTypeApp
            mempty
            ( MTConstructor
                mempty
                "Right"
            )
            (MTPrim mempty MTString)
        )
        `shouldBe` KindArrow KindType KindType
    it "MatchedPair is * -> *" $ do
      doKindCheck (MTConstructor mempty "MatchedPair")
        `shouldBe` KindArrow KindType KindType
    it "MatchedPair String is *" $ do
      doKindCheck (MTTypeApp mempty (MTConstructor mempty "MatchedPair") (MTPrim mempty MTString))
        `shouldBe` KindType
    it "Concrete types are *" $ do
      doKindCheck (MTPrim mempty MTString) `shouldBe` (KindType :: Kind Name)
      doKindCheck
        ( MTFunction
            mempty
            (MTPrim mempty MTString)
            (MTPrim mempty MTString)
        )
        `shouldBe` (KindType :: Kind Name)
      doKindCheck
        ( MTRecord
            mempty
            mempty
        )
        `shouldBe` (KindType :: Kind Name)
      doKindCheck
        ( MTPair
            mempty
            (MTPrim mempty MTString)
            (MTPrim mempty MTString)
        )
        `shouldBe` (KindType :: Kind Name)
