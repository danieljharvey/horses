{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Patterns.PatternsSpec (spec) where

import Calc
import Calc.Patterns.Flatten (generateMissing)
import qualified Data.List.NonEmpty as NE
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = do
  describe "PatternsSpec" $ do
    it "Wildcard is exhaustive" $ do
      let pats = NE.fromList [PWildcard tyInt]
      generateMissing @() pats `shouldBe` []
    it "True needs false" $ do
      let pats = NE.fromList [PLiteral tyBool (PBool True)]
      generateMissing @() pats `shouldBe` [patBool False]
    it "False needs True" $ do
      let pats = NE.fromList [PLiteral tyBool (PBool False)]
      generateMissing @() pats `shouldBe` [patBool True]
    it "False and True needs nothing" $ do
      let pats = NE.fromList [PLiteral tyBool (PBool False), PLiteral tyBool (PBool True)]
      generateMissing @() pats `shouldBe` []
    it "Tuple of two wildcards needs nothing " $ do
      let pats =
            NE.fromList
              [ PTuple
                  (tyTuple [tyBool, tyBool])
                  (PWildcard tyBool)
                  (NE.fromList [PWildcard tyBool])
              ]
      generateMissing @() pats `shouldBe` []

    it "Tuple of one wildcard and one true needs a false" $ do
      let pats =
            NE.fromList
              [ PTuple (tyTuple [tyBool, tyBool]) (PWildcard tyBool) (NE.fromList [PLiteral tyBool (PBool True)])
              ]
      generateMissing @() pats
        `shouldBe` [ patTuple [patBool True, patBool False],
                     patTuple [patBool False, patBool False]
                   ]

    it "Tuple of one true and one false needs a bunch" $ do
      let pats =
            NE.fromList
              [ PTuple (tyTuple [tyBool, tyBool]) (PLiteral tyBool (PBool False)) (NE.fromList [PLiteral tyBool (PBool True)])
              ]
      generateMissing @() pats
        `shouldBe` [ patTuple [patBool False, patBool False],
                     patTuple [patBool True, patBool True],
                     patTuple [patBool True, patBool False]
                   ]

    it "Tuple of booleans with some things supplied" $ do
      let pats =
            NE.fromList
              [ PTuple (tyTuple [tyBool, tyBool]) (PLiteral tyBool (PBool False)) (NE.fromList [PLiteral tyBool (PBool True)]),
                PTuple (tyTuple [tyBool, tyBool]) (PLiteral tyBool (PBool True)) (NE.fromList [PLiteral tyBool (PBool False)])
              ]
      generateMissing @() pats
        `shouldBe` [ patTuple [patBool False, patBool False],
                     patTuple [patBool True, patBool True]
                   ]

    it "Tuple of wildcard and boolean" $ do
      let pats =
            NE.fromList
              [ PTuple (tyTuple [tyBool, tyBool]) (PLiteral tyBool (PBool False)) (NE.fromList [PWildcard tyBool]),
                PTuple (tyTuple [tyBool, tyBool]) (PLiteral tyBool (PBool True)) (NE.fromList [PLiteral tyBool (PBool False)])
              ]
      generateMissing @() pats
        `shouldBe` [ patTuple [patBool True, patBool True]
                   ]
