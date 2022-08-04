{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.Uses
  ( spec,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Modules.Uses
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Modules.Entity
import Language.Mimsa.Types.Typechecker
import Test.Hspec

spec :: Spec
spec = do
  describe "Uses" $ do
    describe "extractUsesTyped" $ do
      it "Finds no types" $ do
        let entities = extractUsesTyped (MyLiteral (MTPrim () MTInt) (MyInt 1))
        entities `shouldSatisfy` S.null
      it "Finds one type" $ do
        let entities = extractUsesTyped (MyVar (MTConstructor () Nothing "Unit") Nothing "a")
        entities `shouldBe` S.fromList [EName "a", EType "Unit"]
      -- ahh fuck it
      xit "Does not find type declared in expression" $ do
        let entities =
              extractUsesTyped
                ( MyData
                    (MTRecord () mempty)
                    (DataType "Unit" mempty (M.singleton "Unit" mempty))
                    (MyVar (MTConstructor () Nothing "Unit") Nothing "a")
                )
        entities `shouldSatisfy` S.null
