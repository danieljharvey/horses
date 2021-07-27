{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Project.Usages
  ( spec,
  )
where

import qualified Data.Set as S
import Language.Mimsa.Project.Usages
import Language.Mimsa.Types.Project
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

spec :: Spec
spec =
  describe "Usages" $ do
    it "Returns empty when passed nothing" $
      findUsages mempty (exprHash 6) `shouldBe` Right mempty
    it "Finds all uses of Compose in Stdlib" $
      findUsages testStdlib (exprHash 6) `shouldBe` Right mempty
    it "Finds direct and transient uses of runState" $ do
      let runStateHash = getHashOfName testStdlib "runState"
          evalStateHash = getHashOfName testStdlib "evalState"
          execStateHash = getHashOfName testStdlib "execState"
          testUsageHash = getHashOfName testStdlib "testStateUsages"
      findUsages testStdlib runStateHash
        `shouldBe` Right
          ( S.fromList
              [ Direct "evalState" evalStateHash,
                Direct "execState" execStateHash,
                Transient "testStateUsages" testUsageHash
              ]
          )
