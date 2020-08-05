{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Usages
  ( spec,
  )
where

import qualified Data.Set as S
import Language.Mimsa.Project.Usages
import Language.Mimsa.Types
import Test.Hspec
import Test.StoreData

spec :: Spec
spec =
  describe "Usages" $ do
    it "Returns empty when passed nothing" $
      findUsages mempty (ExprHash 6) `shouldBe` Right mempty
    it "Finds all uses of Compose in Stdlib" $ do
      let expected =
            S.fromList
              [ Direct (mkName "listHead") (ExprHash 8),
                Direct (mkName "listTail") (ExprHash 9),
                Transient (mkName "list") (ExprHash 10)
              ]
      findUsages stdLib (ExprHash 6) `shouldBe` Right expected
